-- Define a function called removeElems which takes a value as the first argument and a list as the second argument, and outputs the second argument with all instances of the first argument removed.
removeElems x xs = [y | y <- xs, y /= x]  

-- Define a function called longerThan which takes a number as the first argument and a list of lists as its second element. It should output the second argument with only the lists that are longer than the first argument included.  
longerThan x xss = [xs | xs <- xss, length xs > x]

-- Define a function called called printBar which takes a list as its input and outputs a string of *’s with the same length as the input list.
printBar xs = [n | x <- [1 .. length xs], let n = '*']	

-- Define a function called sin’ which approximates sin(x) using the approximation on the next line. The first argument should be the value of x and the second argument should be the number of terms in the approximation. The output should be the resulting value. Note that this will break with a large number of terms because the Double type can’t hold very large factorial values.
-- sin(x) = x – x^3 /3! + x^5 /5! – x^7 /7! + x^9 /9! - ...
sin' x y = sum [((x^n) / (product [1..n])) - ((x^(n + 2)) / (product [1.. n + 2]))  | n <- [1 .. y * 4], n `mod` 4 == 1]
--                          ^ this factorial breaks the function, not sure why, something about print
                        
-- Define a function called toAstro which converts a month and day to the corresponding astrological sign. The inputs should be the month and day as Ints, and the output should correspond to the Signs in the following table. Assume that all months have 31 days, for the sake of this function. That is, months from 1-12 are valid and days from 1-31 are valid for every month.
toAstro m day
	| (m == 12 && day >= 22) || (m == 1  && day <= 19) = "Capricorn"
	| (m == 1  && day >= 20) || (m == 2  && day <= 18) = "Aquaris"
	| (m == 2  && day >= 19) || (m == 3  && day <= 20) = "Pisces"
	| (m == 3  && day >= 20) || (m == 4  && day <= 19) = "Aries" 
	| (m == 4  && day >= 20) || (m == 5  && day <= 20) = "Taurus"
	| (m == 5  && day >= 21) || (m == 6  && day <= 21) = "Gemini"
	| (m == 6  && day >= 22) || (m == 7  && day <= 22) = "Cancer"
	| (m == 7  && day >= 23) || (m == 8  && day <= 22) = "Leo"
	| (m == 8  && day >= 23) || (m == 9  && day <= 22) = "Virgo" 
	| (m == 9  && day >= 23) || (m == 10 && day <= 22) = "Libra"
	| (m == 10 && day >= 23) || (m == 11 && day <= 21) = "Scorpia"
	| (m == 11 && day >= 23) || (m == 12 && day <= 21) = "Sagittarius"

