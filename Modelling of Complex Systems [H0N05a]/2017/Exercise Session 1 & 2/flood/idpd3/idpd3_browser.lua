--[[
The created prototype of the idpd3-ltc binding.
]]
local idpd3_browser = {
  
}

local noLog = {}
function noLog:initialState()
end
function noLog:getOutput()
end
function noLog:getInput()
end
function noLog:noNextState()
end
function noLog:nextState()
end  
function noLog:parseError()
end  

local logPrintRun = {}
function logPrintRun:initialState(state) 
  if(state == nil) then
    print("No intitial state found");
  else 
    print("Got initial state");
  end
end
function logPrintRun:getInput(S_in, T_in, S3)
  if(S_in == nil) then
    print("No output structure found satisfying the input theory");
  else
    print("Got input");
  end
end
function logPrintRun:getOutput(S_out, T_out, S3)
  if(S_out == nil) then 
    print("No output structure found satisfying the draw theory");
  else
    print("Got output");
  end
end
function logPrintRun:noNextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  end
end
function logPrintRun:nextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  else
    print("Got new state");
  end
end
function logPrintRun:parseError(js, table)
  if(table == nil) then
    print("Not parseable as JSON");
  elseif(table.animation == nil) then
    print("Not an animation");
  end
end

local logPrintErrors = {}
function logPrintErrors:initialState(state, S, T_ss, V_ss)
  if(state == nil) then
    Sn = clone(S);
    setvocabulary(Sn, V_ss)
    printcore(T_ss, Sn);
  end
end
function logPrintErrors:getOutput(S_out, T_out, S3)
  if(S_out == nil) then 
    print("No output structure found satisfying the draw theory");
    printcore(T_out, S3);
  end
  print("Got output");
end
function logPrintErrors:getInput(S_in, T_in, Sm)
  if(S_in == nil) then 
    print("No output structure found satisfying the input theory");
    printcore(T_in, Sm);
  end
  print("Got action");
end
function logPrintErrors:noNextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  else
    print("Alternative state found");
  end
end
function logPrintErrors:nextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No new state");
    printcore(T_in, Si);
  end
end
function logPrintErrors:parseError(js, table)
  if(table == nil) then
    print("Not parseable as JSON");
	print(js);
  elseif(table.animation == nil) then
    print("Not an animation");
	print(table);
  end
end

local logPrintStructures = {}
function logPrintStructures:initialState(state, S, T_ss, V_ss)
  if(state == nil) then
    Sn = clone(S);
    setvocabulary(Sn, V_ss);
    printcore(T_ss, Sn);
  else
    print("Got initial state");
    print(S);
  end
end
function logPrintStructures:getOutput(S_out, T_out, S3)
  if(S_out == nil) then 
    print("No output structure found satisfying the draw theory");
    printcore(T_out, S3);
  else
    print("Modelexpand:");
    print(T_out);
    print(S3);
    print("Result:");
    print(S_out);
  end
end
function logPrintStructures:getInput(S_in, T_in, Sm)
  if(S_in == nil) then 
    print("No output structure found satisfying the input theory");
    printcore(T_in, Sm);
  else
    print("Got input");
    print(S_in);
  end
end
function logPrintStructures:noNextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  else
    print("Alternative state found");
    print(Sn);
  end
end
function logPrintStructures:nextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No new state");
    printcore(T_in, Si);
  else
    print("Got next state");
    print(Si);
  end
end
function logPrintStructures:parseError(js, table)
  if(table == nil) then
    print("Not parseable as JSON");
  elseif(table.animation == nil) then
    print("Not an animation");
  end
  print(js);
  print(table);
end

--[[
The creation of an interaction environment.
idpd3_in: The transformer from a lua table to a IDPD3_in structure.
idpd3_out: The transformer from a IDPD3_out structure to a lua table.
json: A json library for decoding and encoding lua tables from and to JSON.
streamIn: The inputstream to read from, defaults to io.stdin.
streamOut: The outputstream to write to, defaults to io.stdout.
]]
function idpd3_browser:create(idpd3_in, idpd3_out, json, streamIn, streamOut) 
  streamIn = streamIn or io.stdin
  streamOut = streamOut or io.stdout
  local out = {log = noLog}
  out.idpd3_in = idpd3_in
  out.idpd3_out = idpd3_out
  out.json = json
  out.streamIn = streamIn
  out.streamOut = streamOut
  setmetatable(out, self)
  self.__index = self
  return out
end

function idpd3_browser:setLogger(log) 
  self.log = assert(log, "Parameter log is nil.");
end

function idpd3_browser:setLogLevel(level) 
  level = assert(level, "Parameter level is nil.");
  if(level<= 0) then
    self.log = noLog;
  elseif(level <= 1) then
    self.log = logPrintRun;
  elseif(level <= 2) then
    self.log = logPrintErrors;
  else
    self.log = logPrintStructures;
  end
end
--[[
The default function to call when no new state can be found.
idpd3_browser: the environment to work in.
S: The last known state.
]]
function defaultStop(idpd3_browser, S)
  local stopQuestion = '{"animation": [{"height":"6","time":1,"elements":'..
  '[{"y":"1","x":"1","key":"revert","color":"green","text_label":"Revert","type":"text"},'..
  '{"y":"1","x":"5","key":"stop","color":"red","text_label":"Stop","type":"text"}],"width":"6"}]}';
  local answer = coroutine.yield(stopQuestion);
  local table = idpd3_browser.json.decode(answer);
  local table = table.animation;
  local elements = table[1].elements;
  local first = elements[1];
  local key = first.key;
  if key == "revert" then
    return S;
  end
  return nil;
end
--[[
Creates a function that runs the LTC-theory interactively. 
It starts with initialising the initial state and outputting it in idpd3 JSON format.
Until no more models can be found it reads from $streamIn$ in JSON, progresses to the next state and outputs the new state to $streamOut$.
When no more models can be found the $noNextState$ function is called with the last known valid state.

T, S, V_state: The LTC-theory, input structure and the state vocabulary.
T_in, S_in, V_in: The theory, structure and vocabulary that translates raw input over the IDPD3 vocabulary to actions interpreted by the LTC-theory.
T_out, S_out, V_out: The theory, structure adn vocabulary that visualizes the state to output.
noNextState(idpd3_browser, S): 
  The function to call when no model can be found. 
  Expects: The last known valid state. 
  Returns: The corrected state or nil to stop.
  Defaults to calling defaultStop with the current idpd3_browser object.
]]
function idpd3_browser:createLTC(
    T, S, V_state,
    T_in, S_in, V_in, 
    T_out, S_out, V_out, noNextState)
  local noNextState = noNextState or function(S) return defaultStop(self, S) end;
  S = clone(S);
  S_in = clone(S_in);
  S_out = clone(S_out);
  setvocabulary(S_in, V_in);
  setvocabulary(S_out, V_out);
  local sys = self:createLTCSys(
      T, S, V_state,
      T_in, S_in, V_in, 
      T_out, S_out, V_out, noNextState)
  sys = coroutine.create(sys);
  local io = self:createIO(sys);
  return io;
end

--[[
Creates a coroutine-function that runs the LTC-theory interactively. It first yields the idpd3 JSON and expects an idpd3 JSON input string.
It loops until no more models can be found and the noNextState function returns nil.

T, S, V_state: The LTC-theory, input structure and the state vocabulary.
T_in, S_in, V_in: The theory, structure and vocabulary that translates raw input over the IDPD3 vocabulary to actions interpreted by the LTC-theory.
T_out, S_out, V_out: The theory, structure and vocabulary that visualizes the state to output.
noNextState(idpd3_browser, S): 
  The function to call when no model can be found. 
  Expects: The last known valid state. 
  Returns: The corrected state or nil to stop.
]]
function idpd3_browser:createLTCSys(
    T, S, V_state,
    T_in, S_in, V_in, 
    T_out, S_out, V_out, noNextState)
  local coIn = function() 
    local m, T_bs, T_ss, V_bs, V_ss = initialise(T, S);
    local state = m[1];
    self.log:initialState(state, S, T_ss, V_ss);
    local Si;
    while state~=nil do
      setvocabulary(state, V_state)
      local So = self:getOutput(state, S_out, T_out, V_out);
      local js_out = self:fromStructure(So);
      local js_in = coroutine.yield(js_out);
      local A = self:toStructure(S_in, js_in);
      Si = self:getInput(state, A, T_in, V_in);
      setvocabulary(Si, V_ss);
      state = self:getNextState(T, Si, noNextState);
    end
    return nil, Si;
  end
  return coIn;
end

--[[
Creates a runnable function binding the i/o communication to the infinite progression loop.

controller: a coroutine-function first yielding an output string and expecting a single input string.
]]
function idpd3_browser:createIO(controller)
  local f = function() 
    local line = nil;
    local cont, js = coroutine.resume(controller, nil);
    self.streamOut:write(js, "\n");
    self.streamOut:flush();
    while self.streamIn:read(0) ~= nil do
      line = self.streamIn:read('*l');
      if line == "exit" then 
        return;
      end
      cont, js, state = coroutine.resume(controller, line);
      if(js == nil) then
        return state;
      end
      self.streamOut:write(js, "\n");
      self.streamOut:flush();
    end
  end
  return f;
end

--[[
Transforms the input JSON string to a idpd3 input structure.
]]
function idpd3_browser:toStructure(defaultStruct, js)
  local table = self.json.decode(js);
  if(table == nil or table.animation == nil) then
    self.log:parseError(js, table);
  end
  local table = assert(table.animation, "Parse error not an animation: " .. js);
  local structure = self.idpd3_in:createStructure(defaultStruct, table);
  return structure;
end
--[[
Transforms the output idpd3 structure to a output JSON string
]]
function idpd3_browser:fromStructure(S)
  local table = self.idpd3_out:getD3(S)
  local table = {animation = table};
  return self.json.encode(table)
end

--[[
Progresses the LTC structure to the first next state, calls noNextState when no state exists.

T: The LTC theory to progess over.
Si: The current state and chosen action.
noNextState(S): 
  A function called with the current state when no new state exists. 
  The returned value is used as the next state if it's not nil.
]]
function idpd3_browser:getNextState(T, Si, noNextState) 
  local Sn = progress(T, Si)[1];
  if(Sn == nil) then
    Sn = noNextState(Si);
    self.log:noNextState(Si, T, Sn);
  end
  self.log:nextState(Si, T, Sn);
  return Sn;
end
--[[
Transforms the current state to a output visualisation structure.

Si: The current state.
S_d3_out: The output structure containing default domains and visualisation predicates.
T_out: The visualisation theory.
V_out: The output vocabulary.
]]
function idpd3_browser:getOutput(Si, S_d3_out, T_out, V_out) 
  setvocabulary(Si, V_out);
  local S3 = merge(Si, S_d3_out);
  --local S3 = merge(S_d3_out, Si);
  local S_out = modelexpand(T_out, S3, V_out)[1];
  self.log:getOutput(S_out, T_out, S3);
  return S_out
end
--[[
Creates the action structure depending on the raw input and the current state.

Si: The current state.
A: The raw input structure.
T_in: The theory to process the input data.
V_in: The vocabulary of the merged state/action. This is usually V_ss of the LTC vocabulary.
]]
function idpd3_browser:getInput(Si, A, T_in, V_in) 
  setvocabulary(Si, V_in);
  setvocabulary(A, V_in);
  local Sm = merge(A, Si);
  local S_in = modelexpand(T_in, Sm)[1];
  self.log:getInput(S_in, T_in, Sm);
  return S_in;
end

return idpd3_browser
