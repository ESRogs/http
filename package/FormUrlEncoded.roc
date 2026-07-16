## application/x-www-form-urlencoded serialization and parsing, matching the
## WHATWG URL spec (and therefore JS `URLSearchParams`).
##
## Serializing: A-Z a-z 0-9 `*` `-` `.` `_` are left bare, space becomes `+`,
## every other UTF-8 byte becomes %XX (uppercase). Parsing is forgiving, as
## the spec requires: malformed percent-escapes pass through literally, and
## invalid UTF-8 decodes with replacement characters.

FormUrlEncoded := [].{

	## Percent-encode a single key or value.
	encode_value : Str -> Str
	encode_value = |s| {
		out = Str.to_utf8(s).fold([], encode_byte)
		match Str.from_utf8(out) {
			Ok(t) => t
			Err(_) => {
				crash "unreachable: form-urlencoded output is always ASCII"
			}
		}
	}

	## Serialize key-value pairs as `k1=v1&k2=v2`, preserving order and duplicates.
	encode_pairs : List((Str, Str)) -> Str
	encode_pairs = |pairs|
		Str.join_with(
			pairs.map(|(k, v)| "${encode_value(k)}=${encode_value(v)}"),
			"&",
		)

	## Parse `k1=v1&k2=v2` into key-value pairs, matching `new
	## URLSearchParams(s)`: segments split on `&` (empty segments are
	## skipped), each segment splits on its first `=` (a missing `=` means an
	## empty value), and keys and values percent-decode via decode_value.
	## Order and duplicate keys are preserved.
	decode_pairs : Str -> List((Str, Str))
	decode_pairs = |s|
		s.split_on("&").keep_if(|segment| segment != "").map(decode_segment)

	## Percent-decode a single key or value: `+` becomes space, `%XX` becomes
	## the byte XX (hex, either case). Malformed percent-escapes are kept
	## literally and invalid UTF-8 decodes lossily, per the WHATWG parser.
	decode_value : Str -> Str
	decode_value = |s|
		Str.from_utf8_lossy(decode_bytes(Str.to_utf8(s), []))

	decode_segment : Str -> (Str, Str)
	decode_segment = |segment|
		match segment.split_on("=") {
			[] => ("", "")
			[key] => (decode_value(key), "")
			[key, .. as rest] => (decode_value(key), decode_value(Str.join_with(rest, "=")))
		}

	decode_bytes : List(U8), List(U8) -> List(U8)
	decode_bytes = |bytes, acc|
		match bytes {
			[] => acc
			['+', .. as rest] => decode_bytes(rest, acc.append(' '))
			['%', hi, lo, .. as rest] =>
				match (hex_value(hi), hex_value(lo)) {
					(Ok(h), Ok(l)) => decode_bytes(rest, acc.append(h.shift_left_by(4).bitwise_or(l)))
					# malformed escape: emit '%' literally and continue from hi
					_ => decode_bytes([hi, lo].concat(rest), acc.append('%'))
				}
			['%', .. as rest] => decode_bytes(rest, acc.append('%'))
			[b, .. as rest] => decode_bytes(rest, acc.append(b))
		}

	encode_byte : List(U8), U8 -> List(U8)
	encode_byte = |acc, b|
		if is_unreserved(b) {
			acc.append(b)
		} else if b == ' ' {
			acc.append('+')
		} else {
			acc.append('%').append(hex_digit(b.shift_right_by(4))).append(hex_digit(b.bitwise_and(15)))
		}

	## The WHATWG urlencoded serializer leaves these bytes bare: A-Z a-z 0-9 * - . _
	is_unreserved : U8 -> Bool
	is_unreserved = |b|
		(b >= '0' and b <= '9')
			or (b >= 'A' and b <= 'Z')
				or (b >= 'a' and b <= 'z')
					or b == '*'
						or b == '-'
							or b == '.'
								or b == '_'

	hex_digit : U8 -> U8
	hex_digit = |n| if n < 10 (n + '0') else (n + 55)

	hex_value : U8 -> Try(U8, [NotHex])
	hex_value = |c|
		if c >= '0' and c <= '9' {
			Ok(c - '0')
		} else if c >= 'A' and c <= 'F' {
			Ok(c - 'A' + 10)
		} else if c >= 'a' and c <= 'f' {
			Ok(c - 'a' + 10)
		} else {
			Err(NotHex)
		}
}

# --- encoding tests, cross-checked against JS `new URLSearchParams(...).toString()` ---

expect FormUrlEncoded.encode_value("") == ""
expect FormUrlEncoded.encode_value("abcXYZ019") == "abcXYZ019"
expect FormUrlEncoded.encode_value("a b") == "a+b"
expect FormUrlEncoded.encode_value("*-._") == "*-._"
expect FormUrlEncoded.encode_value("a&b=c") == "a%26b%3Dc"
expect FormUrlEncoded.encode_value("100%") == "100%25"
expect FormUrlEncoded.encode_value("user@example.com") == "user%40example.com"
expect FormUrlEncoded.encode_value("~!()'") == "%7E%21%28%29%27"
# UTF-8 multibyte: e-acute and snowman
expect FormUrlEncoded.encode_value("é") == "%C3%A9"
expect FormUrlEncoded.encode_value("☃") == "%E2%98%83"

expect FormUrlEncoded.encode_pairs([]) == ""
expect FormUrlEncoded.encode_pairs([("type", "direct")]) == "type=direct"
expect
	FormUrlEncoded.encode_pairs([("to", "general chat"), ("content", "hi & bye")])
		== "to=general+chat&content=hi+%26+bye"
# duplicate keys preserved in order (why this is a List, not a Dict)
expect
	FormUrlEncoded.encode_pairs([("k", "1"), ("k", "2")])
		== "k=1&k=2"

# --- decoding tests, cross-checked against JS `new URLSearchParams(s)` ---

expect FormUrlEncoded.decode_value("a+b") == "a b"
expect FormUrlEncoded.decode_value("a%26b%3Dc") == "a&b=c"
expect FormUrlEncoded.decode_value("100%25") == "100%"
expect FormUrlEncoded.decode_value("%C3%A9") == "é"
expect FormUrlEncoded.decode_value("%E2%98%83") == "☃"
# hex is case-insensitive
expect FormUrlEncoded.decode_value("%c3%a9") == "é"
# malformed escapes pass through literally
expect FormUrlEncoded.decode_value("100%") == "100%"
expect FormUrlEncoded.decode_value("%zz") == "%zz"
expect FormUrlEncoded.decode_value("%2") == "%2"
# a valid escape right after a malformed one still decodes
expect FormUrlEncoded.decode_value("%%41") == "%A"

expect FormUrlEncoded.decode_pairs("") == []
expect FormUrlEncoded.decode_pairs("a=1&b=2") == [("a", "1"), ("b", "2")]
expect FormUrlEncoded.decode_pairs("k=1&k=2") == [("k", "1"), ("k", "2")]
expect FormUrlEncoded.decode_pairs("flag") == [("flag", "")]
expect FormUrlEncoded.decode_pairs("=v") == [("", "v")]
# only the first = splits key from value
expect FormUrlEncoded.decode_pairs("a=b=c") == [("a", "b=c")]
# empty segments are skipped
expect FormUrlEncoded.decode_pairs("a=1&&b=2") == [("a", "1"), ("b", "2")]
expect FormUrlEncoded.decode_pairs("to=general+chat&content=hi+%26+bye") == [("to", "general chat"), ("content", "hi & bye")]

# encode/decode round-trip
expect
	{
		pairs = [("to", "general chat"), ("content", "hi & bye = 100% ☃"), ("k", "1"), ("k", "2")]
		FormUrlEncoded.decode_pairs(FormUrlEncoded.encode_pairs(pairs)) == pairs
	}
