local msg = require "mp.msg"
local utils = require "mp.utils"

local start_time = -1
local end_time = -1

function timestamp(duration)
    local hours = duration / 3600
    local minutes = duration % 3600 / 60
    local seconds = duration % 60
    return string.format("%02d:%02d:%02.03f", hours, minutes, seconds)
end

function osd(str)
    return mp.osd_message(str, 3)
end

function get_destination_filename(srcdir)
   local srcname   = mp.get_property_native("filename")
   local srcnamene = mp.get_property_native("filename/no-ext")

   local ext_length = string.len(srcname) - string.len(srcnamene)
   local srcext     = string.sub(srcname, -ext_length)

   local dstext = srcext

   local dstname
   -- create a new, unique filename by scanning the current
   -- directory for non-existence of files named "excerpt_$number.$extension"

   local direntries = utils.readdir(srcdir)
   local ftable = {}
   for i = 1, #direntries do
      -- mp.msg.log("info", "direntries[" .. i .. "] = " .. direntries[i])
      ftable[direntries[i]] = 1
   end

   local fname = ""
   for i=0,999 do
      local f = string.format("%s_%03d", srcnamene, i)
      -- mp.msg.log("info", "ftable[" .. f .. dstext .. "] = " .. direntries[f .. dstext])

      if ftable[f .. dstext] == nil then
         fname = f
         break
      end
   end
   if fname == "" then
      message = "not writing because all filenames already in use" 
      mp.osd_message(message, 10)
      return ""
   end
   dstname = fname

   return utils.join_path(srcdir, dstname .. dstext)
end


function cut(recode)
    if start_time == -1 or end_time == -1 or start_time >= end_time then
        osd("invalid start and/or end times")
        return
    end

    local extra_flags = ""
    if not recode then
        extra_flags = "-c copy -avoid_negative_ts make_zero"
    end

    -- local cmd = trim(o.command_template:gsub("%s+", " "))
    local inpath = utils.join_path(utils.getcwd(), mp.get_property("path"))
    local srcdir, _ = utils.split_path(inpath)
    local outpath = get_destination_filename(srcdir)
    local template = "ffmpeg -n -ss %f -to %f -i '%s' -map 0 %s '%s'"
    local cmd = string.format(template, start_time, end_time, inpath, extra_flags, outpath)

    os.execute(cmd)
    -- msg.info("done")
end

function cut_recode()
    osd("Re-encode slicing...")
    cut(true)
    osd("done")
end

function cut_copy()
    osd("Copy slicing...")
    cut(false)
    osd("done")
end

function set_slice_start()
    start_time = mp.get_property_number("time-pos", -1)
    osd("Slice Start: " .. timestamp(start_time))
end

function set_slice_end()
    end_time = mp.get_property_number("time-pos", -1)
    osd("Slice End: " .. timestamp(end_time))
end

mp.add_key_binding("w", "set_slice_start", set_slice_start)
mp.add_key_binding("W", "set_slice_end", set_slice_end)
mp.add_key_binding("Ctrl+w", "cut_copy", cut_copy)
mp.add_key_binding("Alt+w", "cut_recode", cut_recode)
