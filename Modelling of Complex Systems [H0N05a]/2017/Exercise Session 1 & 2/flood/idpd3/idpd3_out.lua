--[[
The default output handlers. Reading from the idpd3_out vocabulary and creating a Lua table.
time: 
    This is the list of function that have as parameter 'time'.
    The functions in the vocabulary are prefixed with "d3_".
time_key: 
    This is the list of functions that have as first parameter 'time' and second 'key'.
    The functions in the vocabulary are prefixed with "d3_".
]]
local idpd3_out = {
	time = {"width", "height"}, 
	time_key_func = {
		"type", "x", "y", "order", 
        "circ_r", "rect_width", "rect_height", 
        "color", "text_label", "text_size",
        "link_from", "link_to", "link_width",
        "img_path"
	},
    time_key_pred = {
        "node", "isFixed"
    },
	voc = nil
};

--[[
Creates a new idpd3_out object with an expanded vocabulary.
]]
function idpd3_out:create(voc)
    voc = assert(voc, "Cannot create an idpd3_out object with a nil vocabulary.");
    local out = {voc = voc};
    setmetatable(out, self);
    self.__index = self;
    return out;
end

--[[
Gets a new table from the given structure.

Returns:
    An array of objects indexed by the objects in the domain 'time'.
    These objects are of the form: {time:time, elements: [element], ...}.
    element is of the form: {key:k, type:'...', ...} when a key is used in any time_key partial function.
    element is the union of all function values that have a certain value at the same time and the same key.
]]
function idpd3_out:getD3(S)
    S = assert(S, "Cannot create a table from a nil structure.");
    local d3 = self:createD3();

    for el in elements(S[self.voc.time.type]) do
        self:createD3_time(d3, el);
    end
    --[[
        TODO: rewrite this with nice handlers.
    ]]
    for i,v in ipairs(self.time) do
        self:handleD3_time(d3, v, S);
    end
    for i,v in ipairs(self.time_key_func) do
        self:handleD3_time_key_func(d3, v, S);
    end
    for i,v in ipairs(self.time_key_pred) do
        self:handleD3_time_key_pred(d3, v, S);
    end
	return d3;
end

--[[
Creates a new d3 output table.
]]
function idpd3_out:createD3() 
    local d3 = {}; 
    return d3;
end

--[[
Creates a new d3 time element.
]]
function idpd3_out:createD3_time(d3, time) 
    local state = {time=time, elements={n=0}};
    d3[time] = state;
end

--[[
Adds a new d3 element with the given key to the given state.
]]
function idpd3_out:createD3_element(state, key) 
    if state == nil then
        return nil;
    end
    local element = {key=key};
    local size = state.elements.n+1;
    state.elements[size] = element;
    state.elements.n = size;
    return element;
end

function idpd3_out:handleD3_time(d3, attributeName, S) 
    local pred = "d3_"..attributeName;
    local func_inter = S[self.voc[pred]];
    local table = func_inter.graph.ct;
    for t in tuples(table) do
    	local time = t[1];
    	local value = tostring(t[2]);
    	local state = d3[time]
    	state[attributeName] = value;
    end
end

function idpd3_out:handleD3_time_key_func(d3, attributeName, S)
    local pred = "d3_"..attributeName;
    local func_inter = S[self.voc[pred]];
    local table = func_inter.graph.ct;
    for t in tuples(table) do
        local time = t[1];
        local key = t[2];
        local value = tostring(t[3]);
        local state = d3[time]
        local element = self:getTableWhere(state.elements, "key", key);
        if element == nil then
            element = self:createD3_element(state, key)
        end
        element[attributeName]  = value;
    end
end

function idpd3_out:handleD3_time_key_pred(d3, attributeName, S)
    local pred = "d3_"..attributeName;
    local pred_inter = S[self.voc[pred]];
    local table = pred_inter.ct;
    for t in tuples(table) do
        local time = t[1];
        local key = t[2];
        local value = true;
        local state = d3[time]
        local element = self:getTableWhere(state.elements, "key", key);
        if element == nil then
            element = self:createD3_element(state, key)
        end
        element[attributeName]  = value;
    end
end

--[[
Returns the Lua table from the given array where array[field] == value.
]]
function idpd3_out:getTableWhere(array, field, value) 
    if array ==nil then
    	return nil;
    end
    for k, v in pairs(array) do
    	if k ~= "n" then
            if v[field] == value then
                return v;
            end
        end
    end
    return nil;
end

return idpd3_out;
