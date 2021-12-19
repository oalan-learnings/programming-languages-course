fun is_older(first_date : int*int*int, second_date : int*int*int) = 
  let fun is_list_older(first : int list, second : int list) = 
        if(first = [] andalso second = []) then false
        else if hd first = hd second then is_list_older(tl first, tl second)
        else if hd first < hd second then true
        else false
  in
  is_list_older([#1 first_date, #2 first_date, #3 first_date], [#1 second_date, #2 second_date, #3 second_date])
  end

fun number_in_month(dates : (int*int*int) list, a_month : int) = 
  if(null dates) then 0
  else if(#2 (hd dates) = a_month) then 1 + number_in_month(tl dates, a_month)  
             else 0 + number_in_month(tl dates, a_month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
  if(null months) then 0
  else if(length months = 1) then number_in_month(dates, hd months)
        else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates : (int*int*int) list, a_month : int) = 
  if(null dates) then []
  else if(#2 (hd dates) = a_month) then (hd dates) :: dates_in_month(tl dates, a_month)
        else dates_in_month(tl dates, a_month)

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
  if(null months) then []
  else if(length months = 1) then dates_in_month(dates, hd months)
       else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) = 
  if(n = 1) then hd strings
        else get_nth(tl strings, n - 1)

fun date_to_string(year : int, month : int, day : int) = 
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", 
                        "September", "October", "November", "December"]
  in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end

fun number_before_reaching_sum(sum : int, integers : int list) = 
  if(sum <=  (hd integers + hd (tl integers))) then 1
  else 1 + number_before_reaching_sum(sum - hd integers, tl integers)

fun what_month(a_day : int) =
    let val no_of_days_per_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
      if(a_day mod 365  = 0) then 12
      else if(a_day mod 365 <= 31) then 1 
      else number_before_reaching_sum(a_day mod 365, no_of_days_per_months) + 1
    end

fun month_range(day1 : int, day2 : int) = 
  if(day1 > day2) then []
  else (what_month day1) ::  month_range(day1 + 1, day2)

fun oldest(dates : (int*int*int) list) = 
  if(null dates) then NONE
  else if(null (tl dates)) then SOME (hd dates)
  else if(is_older((hd dates), (hd(tl dates)))) then oldest([(hd dates)] @ tl (tl dates)) 
  else oldest([hd (tl dates)] @ (tl (tl dates)))
