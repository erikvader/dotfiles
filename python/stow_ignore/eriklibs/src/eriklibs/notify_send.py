import subprocess as S

def notify_send(summary, body="", icon=None):
   if not summary:
      raise Exception("summary required")
   args = []
   if icon:
      args.append("--icon={}".format(icon))
   S.run(["notify-send"] + args + [summary, body])
