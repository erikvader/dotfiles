# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class my_edit(Command):
    # The so-called doc-string of the class will be visible in the built-in
    # help that is accessible by typing "?c" inside ranger.
    """:my_edit <filename>

    A sample command for demonstration purposes that opens a file in an editor.
    """

    # The execute method is called when you run this command in ranger.
    def execute(self):
        # self.arg(1) is the first (space-separated) argument to the function.
        # This way you can write ":my_edit somefilename<ENTER>".
        if self.arg(1):
            # self.rest(1) contains self.arg(1) and everything that follows
            target_filename = self.rest(1)
        else:
            # self.fm is a ranger.core.filemanager.FileManager object and gives
            # you access to internals of ranger.
            # self.fm.thisfile is a ranger.container.file.File object and is a
            # reference to the currently selected file.
            target_filename = self.fm.thisfile.path

        # This is a generic function to print text in ranger.
        self.fm.notify("Let's edit the file " + target_filename + "!")

        # Using bad=True in fm.notify allows you to print error messages:
        if not os.path.exists(target_filename):
            self.fm.notify("The given file does not exist!", bad=True)
            return

        # This executes a function from ranger.core.acitons, a module with a
        # variety of subroutines that can help you construct commands.
        # Check out the source, or run "pydoc ranger.core.actions" for a list.
        self.fm.edit_file(target_filename)

    # The tab method is called when you press tab, and should return a list of
    # suggestions that the user will tab through.
    # tabnum is 1 for <TAB> and -1 for <S-TAB> by default
    def tab(self, tabnum):
        # This is a generic tab-completion function that iterates through the
        # content of the current directory.
        return self._tab_directory_content()

class list_copied(Command):
    """
    :list_copied

    Lists all files currently copied in less
    """
    def execute(self):
        copied = [f.path for f in self.fm.copy_buffer]
        self.fm.notify(copied)
        self.fm.execute_console("shell echo -e '" + '\n'.join(copied) + "' | less")

class toggle_flat(Command):
    """
    :toggle_flat

    Flattens or unflattens the directory view.
    """

    def execute(self):
        if self.fm.thisdir.flat == 0:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = -1
            self.fm.thisdir.load_content()
        else:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = 0
            self.fm.thisdir.load_content()

class quick_nav(Command):
    """
    :quick_nav [down|up|_anything_else_]

    Move prefix arg lines up or down and enter the new target if it is
    a directory. If the first argument is neither down or up, prefix
    arg is treated as the absolute position to go to.
    """
    def execute(self):
        # get prefix arg
        if self.quantifier:
            amount = self.quantifier
        else:
            amount = 1

        # move in direction according to first argument
        if self.arg(1) == "down":
            self.fm.move(down=amount)
        elif self.arg(1) == "up":
            self.fm.move(up=amount)
        else:
            self.fm.move(to=amount)

        # enter the thing if it is a directory
        if self.fm.thisfile.is_directory:
            self.fm.move(right=1)
        else:
            self.fm.notify("not a directory, can't enter it!")

class toggle_super_zoom(Command):
    """
    :toggle_super_zoom

    Toggles whether the middle current section should take up the
    whole window.
    """
    def execute(self):
        if hasattr(self.fm, "is_superzoomed") and self.fm.is_superzoomed == True:
            self.fm.is_superzoomed = False
            self.fm.settings.column_ratios = [1,3,4]
            # self.fm.settings.collapse_preview = False
            self.fm.settings.preview_files = True
            self.fm.settings.preview_directories = True
        else:
            self.fm.is_superzoomed = True
            self.fm.settings.column_ratios = [100000,1]
            # self.fm.settings.collapse_preview = False
            self.fm.settings.preview_files = False
            self.fm.settings.preview_directories = False

class toggle_previews(Command):
    """
    """
    def execute(self):
        # if any preview is on
        if (
            not self.fm.settings.collapse_preview or
                self.fm.settings.preview_files or
                self.fm.settings.preview_directories or
                self.fm.settings.preview_images or
                self.fm.settings.use_preview_script # or
                # self.fm.settings.automatically_count_files
           ):
            # turn off
            self.fm.settings.collapse_preview = True
            self.fm.settings.preview_files = False
            self.fm.settings.preview_directories = False
            self.fm.settings.preview_images = False
            self.fm.settings.use_preview_script = False
            # self.fm.settings.automatically_count_files = False
        else:
            # turn on
            self.fm.settings.collapse_preview = False
            self.fm.settings.preview_files = True
            self.fm.settings.preview_directories = True
            self.fm.settings.preview_images = True
            self.fm.settings.use_preview_script = True
            # self.fm.settings.automatically_count_files = True

class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    See: https://github.com/junegunn/fzf
    """
    ONLY_DIR    = "d" #only list directories
    EXECUTE     = "e" #run default action on selected thing
    RECURSIVE   = "r" #run recursively
    HIDDEN      = "h" #show hidden files
    EXECUTE_DIR = "E" #enter if directory
    def __init__(self, *args, **kwargs):
        super(fzf_select, self).__init__(*args, **kwargs)
        self.flags, _ = self.parse_flags()

    def execute(self):
        import subprocess
        from os import environ

        command = r"find -L . %s \( -fstype dev -o -fstype proc %s \) -prune -o %s -print 2>/dev/null | sed 1d | cut -b3-"
        command = environ.get("FZF_BASE_COMMAND", command)

        pftuple = ["", "", ""]

        if self.ONLY_DIR in self.flags:
            pftuple[2] += "-type d "
        if self.RECURSIVE not in self.flags:
            pftuple[0] += "-maxdepth 1 "
        if self.HIDDEN not in self.flags:
            pftuple[1] += "-o -path '*/.*' "

        command = command % tuple(pftuple)
        command += " | fzf +m --reverse"
        command = command.replace(r"\\", "\\")

        fzf = self.fm.execute_command(command, universal_newlines=True, stdout=subprocess.PIPE)
        stdout, _ = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            self.fm.select_file(fzf_file)
            if self.EXECUTE in self.flags:
                self.fm.move(right=1)
            elif self.EXECUTE_DIR in self.flags:
                if os.path.isdir(fzf_file):
                    self.fm.cd(fzf_file)

class mkcd(Command):
    """
    :mkcd <dirname>

    Creates a directory with the name <dirname> and enters it.
    """

    def execute(self):
        from os.path import join, expanduser, lexists
        from os import makedirs
        import re

        dirname = join(self.fm.thisdir.path, expanduser(self.rest(1)))
        if not lexists(dirname):
            makedirs(dirname)

            match = re.search('^/|^~[^/]*/', dirname)
            if match:
                self.fm.cd(match.group(0))
                dirname = dirname[match.end(0):]

            for m in re.finditer('[^/]+', dirname):
                s = m.group(0)
                if s == '..' or (s.startswith('.') and not self.fm.settings['show_hidden']):
                    self.fm.cd(s)
                else:
                    ## We force ranger to load content before calling `scout`.
                    self.fm.thisdir.load_content(schedule=False)
                    self.fm.execute_console('scout -ae ^{}$'.format(s))
        else:
            self.fm.notify("file/directory exists!", bad=True)
