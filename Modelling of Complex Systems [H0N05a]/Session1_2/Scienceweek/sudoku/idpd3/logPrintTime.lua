local printTime = {timer=timer}

function printTime:setTimer(timer) 
  self.timer = timer;
  return self;
end

function printTime:create()
  local out = {}
  local mt = getmetatable(self) or self;
  setmetatable(out, mt)
  self.__index = self
  out.timer = self.timer:start();
  return out;
end

function printTime:initialState(state) 
  if(state == nil) then
    print("No intitial state found");
  else 
    print("Got initial state");
  end
  self.timer:printTime();
  self.timer:restart();
end
function printTime:getInput(S_in, T_in, S3)
  if(S_in == nil) then
    print("No output structure found satisfying the input theory");
  else
    print("Got input");
  end
  self.timer:printTime();
  self.timer:restart();
end
function printTime:getOutput(S_out, T_out, S3)
  if(S_out == nil) then 
    print("No output structure found satisfying the draw theory");
  else
    print("Got output");
  end
  self.timer:printTime();
  self.timer:restart();
end
function printTime:noNextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  end
  self.timer:printTime();
  self.timer:restart();
end
function printTime:nextState(Si, T_in, Sn)
  if(Sn == nil) then
    print("No nextState");
  else
    print("Got new state");
  end
  self.timer:printTime();
  self.timer:restart();
end

return printTime