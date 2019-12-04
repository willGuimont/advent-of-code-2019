xs = ('108457'..'562041').map { |x| x }

def split_array(arr, f)
  out = [[]]
  i = 0
  arr.each do |x|
    if not f.call x
      out[i].push x
    else
      out.push []
      i += 1
    end
  end
  out
end

password = xs.map do |x|
  x.scan /\w/
end.select do |x|
  pairs = x[0..-2].zip(x[1..-1])
  pairs.all? do |a, b|
    b >= a
  end
end

password = password.select do |x|
  pairs = x[0..-2].zip(x[1..-1])
  y = pairs.map do |a, b|
    a == b
  end
  y = split_array(y, -> (x) {x == false})
  y.any? {|a| a.length == 1}
end

print password.length
