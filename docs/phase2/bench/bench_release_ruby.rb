reps_fast = 3_000_000
reps_mid = 400_000
reps_slow = 200

# map / filter / reduce
sum_map = (1..reps_fast).inject(0, :+)
sum_filter = (0...reps_fast).select { |i| i.even? }.inject(0, :+)

# hash-map / conj / nth / rest / interleave / count / empty?
sum_collections = 0
reps_mid.times do |x|
  m = { a: x, b: x }
  v = [x, x]
  n0 = v[0]
  r = v[1..]
  i = []
  len = [v.length, r.length].min
  (0...len).each do |idx|
    i << v[idx]
    i << r[idx]
  end
  c = i.length
  sum_collections += m.empty? ? 0 : c + n0
end

# remove / drop-while / keep / keep-indexed
sum_keep = 0
idx = 0
(0...reps_mid).each do |i|
  next unless i.even?
  next if i < 10
  sum_keep += i if (idx % 2).zero?
  idx += 1
end

# sort / sort-by / some
small = (0...20_000).to_a
sum_sort = 0
reps_slow.times do
  sorted = small.sort
  sortby = small.sort_by { |x| -x }
  some_flag = small.any?(&:even?) ? 1 : 0
  sum_sort += sorted.length + sortby.length + some_flag
end

# split / join
text = "a,b,c,d,e,f,g,h,i,j"
sum_text = 0
reps_mid.times do
  parts = text.split(",")
  joined = parts.join("|")
  sum_text += joined.length + parts.length
end

total = sum_map + sum_filter + sum_collections + sum_keep + sum_sort + sum_text
puts total
