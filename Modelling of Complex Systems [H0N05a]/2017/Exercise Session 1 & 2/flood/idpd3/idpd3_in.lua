--[[
The default input handlers. Writing to a structure with a idpd3_in vocabulary.

handlers: The functions to call for each type.
close: The predicates/functions to make 2 valued when finished.
voc: The vocabulary of the output structure to build around.
]]
local idpd3_in = {
	handlers = {},
	close = {"d3_click"},
	voc = nil
}

--[[
Creates a new idpd3 object over a different vocabulary.
]]
function idpd3_in:create(voc) 
    local out = {voc = voc};
    setmetatable(out, self);
    self.__index = self;
    return out;
end

--[[
Adds a handler to this object.

type: The type of element to apply the handler to.
handler: 
	The function to give the element to. 
	The parameters are: self, time and element.
]]
function idpd3_in:addHandler(type, handler) 
	self.handlers[type] = handler
end

--[[
Creates the new structure based on a clone of the input structure and the input data.
]]
function idpd3_in:createStructure(structOut, data)
	local build = self:buildS_D3(structOut)
	build:handleData(data)
	return build:finishS_D3()
end

function idpd3_in:handleData(data) 
	for i,d in ipairs(data) do
		self:handleD(d)
	end
end

function idpd3_in:buildS_D3(struct)
	local newS = clone(struct);
	local out = {struct = newS}
	setmetatable(out, self)
	self.__index = self
	return out
end

function idpd3_in:finishS_D3() 
	local s = self.struct
	for i,c in ipairs(self.close) do
		local v = self.voc[c]
		local pred = s[v]
		pred.pt = pred.ct
	end
	self.struct = nil
	return s
end

function idpd3_in:handleD(d) 
	local time = d.time
	local elements = d.elements
	for i,e in ipairs(elements) do
		local t = e.type
		local handle = self.handlers[t]
		if handle ~= nil then 
			handle(self, time, e)
		end
	end
end

function idpd3_in:handleClick(time, element) 
	local key = element.key;
	local p = self.voc["d3_click"];
	local pred = self.struct[p]
	maketrue(pred, {time, key})
end

idpd3_in:addHandler("click", idpd3_in.handleClick);
return idpd3_in
