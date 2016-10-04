local logDispatch = {}

function logDispatch:create()
  local out = {}
  local mt = getmetatable(self) or self;
  setmetatable(out, mt)
  self.__index = self
  out.N = 0;
  return out;
end
function logDispatch:addLog(log) 
  self.N = self.N + 1;
  self[self.N] = log;
  return self;
end

function logDispatch:initialState(state) 
  for i = 1,self.N do 
    self[i]:initialState(state);
  end
end
function logDispatch:getInput(S_in, T_in, S3)
  for i = 1,self.N do 
    self[i]:getInput(S_in, T_in, S3)
  end
end
function logDispatch:getOutput(S_out, T_out, S3)
  for i = 1,self.N do 
    self[i]:getOutput(S_out, T_out, S3)
  end
end
function logDispatch:noNextState(Si, T_in, Sn)
  for i = 1,self.N do 
    self[i]:noNextState(Si, T_in, Sn)
  end
end
function logDispatch:nextState(Si, T_in, Sn)
  for i = 1,self.N do 
    self[i]:nextState(Si, T_in, Sn)
  end
end

return logDispatch