local function bad(idpd3_check, S)
  print("Bad solution found.");
  print(S)
  idpd3_check:draw(S);
end

local function correct(idpd3_check, S)
  print("Correct solution.");
  idpd3_check:draw(S);
end

local function notEnough(idpd3_check, first, T, S, count, expected)
  if(count == 0) then
    print("No models found. Theory too constrained, searching for an explanation.");
    printcore(T,S)
  elseif count < expected then
    print("Expected ",tostring(expected)," but only ",tostring(count)," models found.");
    idpd3_check:draw(first);
  else
    print("Found more models than expected, all correct.")
    idpd3_check:draw(first);
  end
end

local function table_shallow_copy(t)
  local t2 = {}
  for k,v in pairs(t) do
    t2[k] = v
  end
  return t2
end


--[[
The created prototype of the idpd3_check binding.
]]
local idpd3_check = {}

--[[
The creation of an interaction environment.
idpd3_out: The transformer from a IDPD3_out structure to a lua table.
json: A json library for decoding and encoding lua tables from and to JSON.
streamOut: The outputstream to write to, defaults to io.stdout.
]]
function idpd3_check:create(idpd3_out, json, streamOut) 
  streamOut = streamOut or io.stdout
  local out = {}
  out.idpd3_out = idpd3_out
  out.json = json
  out.streamOut = streamOut
  out.expModels = 1;
  out.bad = bad;
  out.correct = correct;
  out.notEnough = notEnough;
  local mt = getmetatable(self) or self;
  setmetatable(out, mt)
  self.__index = self
  return out
end

function idpd3_check:problem(T_exp, T_out, S_out, V_out, nbModels)
  local out = table_shallow_copy(self);
  out.T_exp = assert(T_exp, "No solution theory passed.");
  out.T_out = assert(T_out, "No drawing theory passed.");
  out.S_out = assert(S_out, "No drawing structure passed.");
  out.V_out = assert(V_out, "No drawing vocabulary passed.");
  out.expModels = nbModels or self.expModels;
  local mt = getmetatable(self) or self;
  setmetatable(out, mt)
  --out.__index = self;
  return out;
end 

function idpd3_check:run(T, S)
  T = assert(T, "No test theory passed.");
  S = assert(S, "No input structure passed");
  local firstModel, count, badModel = self:findBad(T, S);
  if(badModel ~= nil) then
    self:bad(badModel);
  elseif count == self.expModels then
    self:correct(firstModel, count);
  else
    self:notEnough(firstModel, T, S, count, self.expModels);
  end
end

function idpd3_check:findBad(T, S)
  local iter = modelIterator(T, S);
  local model = iter();
  local firstModel = model;
  local badModel = nil;
  local count = 0;
  while (model ~= nil and count <= self.expModels + 1) do
    if(not self:check(model)) then
      badModel = model;
      break;
    end
    model = iter();
    count = count + 1;
  end
  return firstModel, count, badModel;
end

function idpd3_check:check(S)
  local iter = modelIterator(self.T_exp, S)
  --printcore(self.T_exp, S);
  return iter() ~= nil;
end

function idpd3_check:draw(S)
  S = assert(S, "Cannot draw a nil structure.");
  local S_out = self:getOutput(S, self.S_out, self.T_out, self.V_out);
  local js = self:fromStructure(S_out);
  self.streamOut:write(js, "\n");
  self.streamOut:flush();
end

--[[
Transforms the output idpd3 structure to a output JSON string
]]
function idpd3_check:fromStructure(S)
  S = assert(S, "Cannot create JSON from a nil structure.");
  local table = self.idpd3_out:getD3(S);
  table = {animation = table};
  return self.json.encode(table);
end

--[[
Transforms the current state to a output visualisation structure.

Si: The current state.
S_d3_out: The output structure containing default domains and visualisation predicates.
T_out: The visualisation theory.
V_out: The output vocabulary.
]]
function idpd3_check:getOutput(Si, S_d3_out, T_out, V_out) 
  setvocabulary(Si, V_out);
  local S3 = merge(Si, S_d3_out);
  --local S3 = merge(S_d3_out, Si);
  --local S_out = calculatedefinitions(T_out, S3);
  local S_out = modelexpand(T_out, S3, V_out)[1];
  print(S_out);
  if(S_out == nil) then 
    print("No output structure found satisfying the draw theory.");
    printcore(T_out, S3);
  end
  print("Got output");
  return S_out
end

return idpd3_check;
