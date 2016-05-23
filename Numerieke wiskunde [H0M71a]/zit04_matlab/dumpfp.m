function [varargout,dec] = dumpfp(d)

old_matlab = all(sprintf('%bx', 1+eps) == '010000000000f03f');
if old_matlab
    s = sprintf('%bx', d);
else
    % new Matlab does give this hex string in reverse order,
    % also changes in char() function for bytereversion
    s = fliplr(sprintf('%bx', d));
end;
ss = zeros(size(s));
ss([1:2:end-1]+1) = s([1:2:end-1]); % swap bytes
ss([1:2:end-1]) = s([1:2:end-1]+1); %
if old_matlab, s = char(ss); end;

man1_hex = s([1:8]);
man0_hex = s([1:5]+8);
expn_sgnb_hex = s([1:3]+13);

P = fliplr(eye(4));
man1_bin = fliplr(char(double(dec2bin(hex2dec(man1_hex), 32)) * kron(eye(32/4), P)));
man0_bin = fliplr(char(double(dec2bin(hex2dec(man0_hex), 20)) * kron(eye(20/4), P)));
expn_sgnb_bin = fliplr(char(double(dec2bin(hex2dec(expn_sgnb_hex), 12)) * kron(eye(12/4), P)));
expn_bin = expn_sgnb_bin(2:end);
sgnb_bin = expn_sgnb_bin(1);

expn = bin2dec(expn_bin)-1023;
n = '1.';
if expn == -1023, expn_str = 'denorm'; n = '0.';
elseif expn == 1024, expn_str = 'Inf/NaN'; n = 'x.';
else expn_str = sprintf('2^%d', expn);
end;
if sgnb_bin == '1', sgnb = '-';
else sgnb = '+';
end;
man_bin = [man0_bin man1_bin];

if nargout == 0
    fprintf('% -24.17g %s%s%s 2^%s (%s)\n', d, sgnb, n, man_bin, ...
            expn_bin, expn_str);
else
    varargout(1) = {sgnb_bin};
    varargout(2) = {n};
    varargout(3) = {man_bin};
    varargout(4) = {expn_bin};
    varargout(5) = {expn};
end;


dd=0;
if n~=0
    dec=2^(expn);
    for i=1:52,
        dd(i) = sscanf(man_bin(i)', '%d', [1, inf]);
        dec=dec+dd(i)*2^(expn-i);
    end
end
dd;
        