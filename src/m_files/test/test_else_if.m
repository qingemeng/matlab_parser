function test_elseif
bob=42
  if bob == 42
    a = 5;
  end

  if ( bob == 42 )
        a = 6
  else
      if ( bob == 43 )
         a = 7

      else
          if bob == 44
        a = 8
          else
        b = 100
          end
      end
    %b = @frank;
  end
end

