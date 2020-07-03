
def fzf_dmenu(choices, fzf_options=None):
   """
   choices is a list of strings or a multiline string of things to choose between.
   fzf_options is a list of strings of arguments to pass to fzf_dmenu.

   returns None if nothing was selected
   """
   import subprocess as P

   if isinstance(choices, str):
      inp = choices
   elif isinstance(choices, list):
      inp = "\n".join(choices)
   else:
      raise Exception("`choices` needs to either be of type str or list")

   res = P.run(
      ["fzf_dmenu"] + (fzf_options if fzf_options is not None else []),
      input=inp,
      text=True,
      stdout=P.PIPE
   )

   if res.stdout:
      return res.stdout.rstrip("\n")
   else:
      return None
