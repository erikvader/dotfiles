local msg = require "mp.msg"
local utils = require "mp.utils"

local cut_pos = nil

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


function cut(shift, endpos)
    -- local cmd = trim(o.command_template:gsub("%s+", " "))
    local inpath = utils.join_path(utils.getcwd(), mp.get_property("path"))
    local srcdir, _ = utils.split_path(inpath)
    local outpath = get_destination_filename(srcdir)
    local template = "ffmpeg -n -i '%s' -ss %f -map 0 -c copy -to %f '%s'"
    local cmd = string.format(template, inpath, shift, endpos, outpath)

    msg.info(cmd)
    os.execute(cmd)
    msg.info("done")
end

function toggle_mark()
    local pos = mp.get_property_number("time-pos")
    if cut_pos then
        local shift, endpos = cut_pos, pos
        if shift > endpos then
            shift, endpos = endpos, shift
        end
        if shift == endpos then
            osd("Cut fragment is empty")
        else
            cut_pos = nil
            osd(string.format("Cut fragment: %s - %s",
                timestamp(shift),
                timestamp(endpos)))
            cut(shift, endpos)
        end
    else
        cut_pos = pos
        osd(string.format("Marked %s as start position", timestamp(pos)))
    end
end

mp.add_key_binding("w", "slicing_mark", toggle_mark)
