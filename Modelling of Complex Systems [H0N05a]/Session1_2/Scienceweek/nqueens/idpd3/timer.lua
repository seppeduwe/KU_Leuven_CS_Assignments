
local timer = {}

function timer:start() 
  local out = {};
  local mt = getmetatable(self) or self;
  setmetatable(out, mt)
  self.__index = self
  out:restart();
  return out;
end

function timer:restart()
  self.start = os.clock();
end

function timer:getTime()
  local diff = os.clock() - self.start;
  return diff;
end

function timer:printTime(p)
  if(p~=nil) then
    print(p);
  end
  local e = self:getTime();
  print("Time: "..e);
end

return timer;