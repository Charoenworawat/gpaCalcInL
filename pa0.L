(* 
Student 1: Jetharin Kevin Charoenworawat; Kevin.Charoenworawat@gmail.com; JKC2237; KChar
Student 2: Jack Medellin; jackmedellin12@gmail.com; JCM4382; medellin
*)



fun read_weights with n =
	if n = 0 then 0 else
	let _ = print "Please enter a weight:" in
	let val = readInt in
	let conditional = if ( (val > 100) | (val < 0)) then 
	(let _ = print "Input out of range" in
	(read_weights n) ) else 
	(let rest = (read_weights (n-1)) in
	val@rest)
	in (conditional)
in
fun sum_list with list = 
	if isNil #list then !list else
	let head = !list in
	let tail = #list in
	(sum_list tail) + head
in
fun check_num_grades with g =
	if g >= 0 then g else 0
in
fun check_sum with s =
	if s = 100 then (get_students "Y") else
	(let con = if s = 0 then print "Error: Not a valid input for number of assignments" else
	print "Error: Weights do not add up to 100" in
	fun doNothing with nothing =
		if nothing = "nothin" then 0 else 0
	in (doNothing "nothin"))
in
fun get_students with c =
	if c = "Y" then 
	 (let _ = print "Please enter the name of the student:" in
	 let sName = readString in
	 fun read_grades with n =
		if n = 0 then Nil else
		let _ = print "Please enter a grade of the student:" in
		let val = readInt in
		let conditional = if ( (val > 100) | (val < 0)) then 
		(let _ = print "Input out of range" in
		(read_grades n) ) else 
		let rest = (read_grades (n-1)) in
		val@rest
		in (conditional)
	in
	let sGrades = (read_grades numGrades) in
	fun calc with studentGrades, list_weights = 
		if isNil !studentGrades then 0 else
		let grade =!studentGrades in
		let weight = !list_weights in
		( (grade * weight) + (calc #studentGrades #list_weights))
	in
	let sAverage = ((calc sGrades weights)/100 ) in
	let iteration_results = (sName@sAverage) in
	fun repeat with j= 
		if j = 1 then
		(let _ = print "Do you want to enter another student? (Y/N)" in
		let yesNo = readString in
		yesNo) else ""
	in 
	let result_repeat = (repeat 1) in
	(iteration_results@(get_students result_repeat))  )
	else ""
in
let _ = print "Enter number of assignments:" in
let numGrades = readInt in
let result_check = (check_num_grades numGrades) in
let weights = (read_weights result_check) in
let sum = (sum_list weights) in
let results = (check_sum sum) in
fun end with results =
	if isNil #results then 0 else
	let group = !results in
	let remainder = #results in
	let name = !group in
	let avg = #group in
	let _ = print name in
	let _ = print avg in
	(end remainder)
in
(end results)
