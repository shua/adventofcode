input = '{"a":2, "b":{"e":2,"c":"red"},"c":"hey","d":[1,"red"]}'

data = JSON.parse input

console.log data

jsum = (o) -> 
	switch (typeof o)
		when "object"
			ret = 0
			for c of o
				if o.constructor isnt Array and o[c] is "red"
					return 0
				ret += jsum o[c]
			return ret
		when "number" then return o
		else return 0

console.log jsum data
