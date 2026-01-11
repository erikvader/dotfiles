# pyright: strict
import subprocess as S
from typing import Callable, Any, Concatenate, Self
from .threadingtools import fork, CancelToken
import logging
from dataclasses import dataclass

logger = logging.getLogger(__name__)


class SpawningPoolError(Exception):
    """General error of a spawnling, parent to all the other"""


class FailedToSpawnError(SpawningPoolError):
    """Failure when spawning, did not even start"""

    def __init__(self, oserr: OSError):
        super().__init__(str(oserr), "Is it installed?")


class NotSuccessfulError(SpawningPoolError):
    """A process did not exit successfully"""


@dataclass(frozen=True)
class Corpse:
    progname: str
    exitcode: int
    stdout: str
    stderr: str

    def was_successful(self) -> bool:
        return self.exitcode == 0

    def check(self) -> Self:
        if not self.was_successful():
            raise NotSuccessfulError(
                f"{self.progname} did not exit successfully: {self.exitcode}"
            )
        return self


class Spawnling:
    def __init__(self, proc: S.Popen[str], name: str):
        self._proc = proc
        self._name = name
        logger.debug("Spawned %s", self)

    def _log_exit(self):
        if self._proc.returncode is None:
            s = "did not exit?"
        else:
            s = f"exited with {self._proc.returncode}"
        logger.debug("%s %s", self, s)

    @classmethod
    def spawn(cls, progname: str, *args: str):
        """Spawn a spawnling of progname with args"""
        try:
            proc = S.Popen(
                [progname] + list(args),
                text=True,
                stdin=S.DEVNULL,
                stdout=S.PIPE,
                stderr=S.PIPE,
            )
        except OSError as e:
            raise FailedToSpawnError(e) from None
        else:
            return cls(proc, f"{progname}[{proc.pid}]")

    def quick(self) -> Corpse:
        """Treat as a short-lived program and wait for it to exit.

        This is good for programs that do one thing quickly, produce relatively short output
        and then dies. It also doesn't do much if it receives a SIGKILL if it doesn't respond
        quickly to SIGINT (keyboard interrupt).

        This is essentially the same as subprocess.run, except with hardcoded arguments and
        throws different exceptions.

        """
        try:
            # NOTE: this part is basically copy paste from the original subprocess.run
            with self._proc:
                try:
                    # NOTE: this thing will wait a short while on keyboardinterrupt
                    stdout, stderr = self._proc.communicate()
                except:
                    self.critical_hit()
                    raise
        finally:
            self._log_exit()

        return Corpse(
            progname=str(self),
            exitcode=self._proc.returncode,
            stdout=stdout,
            stderr=stderr,
        )

    def heavy(
        self,
        stdout_callback: Callable[Concatenate[str, ...], None] = logger.info,
        stderr_callback: Callable[Concatenate[str, ...], None] = logger.error,
    ) -> Corpse:
        """Treat as a long-lived program, wait for it exit while logging its output.

        This is good from programs that live a long time while doing some kind of service.
        This will not receive a sigkill on keyboard interrupt, the process will continue
        to run. It is assumed that these programs handle a lot of state and need to do
        proper cleanup, so they are not force killed automatically.

        """

        def waiter(_c: CancelToken) -> Any:
            return self._proc.wait()

        def stdout_printer(_c: CancelToken) -> Any:
            assert self._proc.stdout is not None
            try:
                for line in self._proc.stdout:
                    stdout_callback("%s stdout: %s", str(self), line)
            finally:
                self._proc.stdout.close()

        def stderr_printer(_c: CancelToken) -> Any:
            assert self._proc.stderr is not None
            try:
                for line in self._proc.stderr:
                    stderr_callback("%s stderr: %s", str(self), line)
            finally:
                self._proc.stderr.close()

        try:
            ret = fork(
                waiter,
                stdout_printer,
                stderr_printer,
                # NOTE: prevent keyboardinterrupts on any function, all of them need to finish
                # by the process exiting. Also assume that a keyboard interrupt means a sigint
                # was sent to the subprocess, so no cancel_callback with a terminate
                reuse_current_thread=False,
                thread_prefix_name="spawningpool",
            )
        finally:
            self._log_exit()

        exitcode = ret[0]
        assert isinstance(exitcode, int)

        return Corpse(progname=str(self), exitcode=exitcode, stdout="", stderr="")

    def critical_hit(self):
        """Kill immediately, no chance for cleanup"""
        logger.error("Sending SIGKILL to %s", self)
        self._proc.kill()

    def damage(self):
        """Inflict damage, it will die on its own"""
        logger.warning("Sending SIGTERM to %s", self)
        self._proc.terminate()

    def __str__(self):
        return self._name
