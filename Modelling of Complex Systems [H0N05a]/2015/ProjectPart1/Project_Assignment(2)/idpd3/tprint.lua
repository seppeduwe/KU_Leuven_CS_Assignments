--
--  tprint.lua

--[[

For debugging what tables have in them, prints recursively

See forum thread:  http://www.gammon.com.au/forum/?id=4903

eg.

require "tprint"

tprint (some_table)

--]]

function tprint (tbl, maxDepth, indent)
	local maxDepth = maxDepth or 10
	local indent = indent or 0
	local formatting = string.rep(" ", indent)
	if indent >= maxDepth then
		print(formatting .. tostring(tbl));
		return
	end
	for k, v in pairs(tbl) do
		local f = formatting .. k .. ": ";
		if type(v) == "table" then
			print(f)
			tprint(v, maxDepth, indent+1)
		else
			print(f .. tostring(v))
		end
	end
end
return tprint
