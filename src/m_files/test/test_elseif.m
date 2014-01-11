function test_elseif
bob=42
  if bob == 42
    a = 5;
  end

  if ( bob == 42 )
        a = 6
%   elseif ( bob == 43 )
%         a = 7
%   elseif bob == 44
%         a = 8
  else
     b = 100
    %b = @frank;
  end
end