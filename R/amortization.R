#' monthly_payment
#'
#' @param principle
#' @param annual_interest
#' @param years
#'
#' @return
#' @export
#'
#' @examples
monthly_payment <- function(principle, annual_interest, years){
  #### determine monthly payment
  # source for formula:
  # https://www.myamortizationchart.com/articles/how-is-an-amortization-schedule-calculated/

  monthly_interest <- annual_interest/12
  n_payments <- years * 12
  month_pay = (monthly_interest * principle * (1 + monthly_interest)** n_payments) /
    ( (1 + monthly_interest)**n_payments - 1 )
  return(month_pay)
}



#' length_mortgage
#'
#' @param principle
#' @param annual_interest
#' @param month_payment
#'
#' @return
#' @export
#'
#' @examples
length_mortgage <- function(principle, annual_interest,month_payment){
  #### determine monthly payment
  # source for formula:
  # https://www.myamortizationchart.com/articles/how-is-an-amortization-schedule-calculated/

  monthly_interest <- annual_interest/12
  #n_payments <- years * 12
  n_payments <- log10(month_payment/(month_payment - monthly_interest * principle))/log10(1 + monthly_interest)

  return(n_payments)
}



#' amort_table
#'
#' @param principle
#' @param annual_interest
#' @param years
#'
#' @return
#' @export
#'
#' @examples
amort_table <- function(principle, annual_interest, years){
  monthly_interest <- annual_interest/12
  n_payments  <- years * 12
  month_pay <- monthly_payment( principle, annual_interest, years)
  tab <-  create_table(1:n_payments,principle, annual_interest, month_pay, n_payments)

  for(i in 1:(NROW(tab)-1)){
    tab$principle[i+1] <- tab$balance[i]
    tab$interest_portion[i+1] <- tab$principle[i+1] * monthly_interest
    tab$principle_portion[i+1] <- tab$month_payment[i+1] - tab$interest_portion[i+1]
    tab$balance[i+1] <- tab$principle[i+1] - tab$principle_portion[i+1]
    tab$cumulative_int[i+1] <- tab$interest_portion[i+1] + tab$cumulative_int[i]
    tab$cumulative_principle[i+1] <- tab$principle_portion[i+1] + tab$cumulative_principle[i]
    tab$cumulative_total[i+1] <- tab$month_payment[i+1] + tab$cumulative_total[i]
    tab$projected_n_left[i+1] <- tab$projected_n_left[i] - 1
  }

  return(tab)
}



#' create_table
#'
#' @param payment
#' @param start_principle
#' @param annual_interest
#' @param month_pay
#' @param n_payments
#'
#' @return
#' @export
#'
#' @examples
create_table <- function(payment, start_principle, annual_interest, month_pay, n_payments){
  tab <- tibble::tibble(payment = payment,
                        principle = start_principle,
                        interest_rate = annual_interest,
                        month_payment = month_pay) %>% dplyr::mutate(
                          interest_portion = principle * annual_interest/12,
                          principle_portion = month_payment - interest_portion,
                          balance = principle - principle_portion,
                          cumulative_int = interest_portion,
                          cumulative_principle = principle_portion,
                          cumulative_total = month_payment,
                          projected_n_left = n_payments - 1
                        )
  tab
}



#' amort_event_table
#'
#' @param principle
#' @param annual_interest
#' @param years
#' @param events
#'
#' @return
#' @export
#'
#' @examples
amort_event_table <- function(principle, annual_interest, years, events = NULL){
  monthly_interest <- annual_interest/12
  n_payments  <- years * 12
  month_pay <- monthly_payment(principle, annual_interest, years)

  tab <- create_table(payment = 1, principle, annual_interest, month_pay, n_payments)
  i <- 1
  while(dplyr::last(tab$balance) > 0){

    # calculate the numbers for the new row
    new_payment <- tab$payment[i] + 1
    new_principle <- tab$balance[i]
    new_month_payment <- tab$month_payment[i]
    new_interest_portion <- new_principle * monthly_interest
    new_principle_portion <- new_month_payment - new_interest_portion
    new_balance <- new_principle - new_principle_portion
    new_projected_n_left <- tab$projected_n_left[i] - 1

    # update the numbers if an event happens this payment
    if(new_payment %in% events$payment){
      # first update changes to principle
      # then update changes to interest rate
      # then update changes to monthly (but need )

      # if principle changes need to up date
      if(!is.na(events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(principle))){
        new_principle <- new_principle - events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(principle)
        new_interest_portion <- new_principle * monthly_interest
        new_principle_portion <- new_month_payment - new_interest_portion
        new_balance <- new_principle - new_principle_portion
        new_projected_n_left <- length_mortgage(principle = new_principle, annual_interest = annual_interest, month_pay = new_month_payment)
      }



      #if interest rate changes need to update
      if(!is.na(events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(annual_interest))){
        annual_interest <- events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(annual_interest)
        monthly_interest  <- annual_interest/12
        new_month_payment <- monthly_payment(principle = new_principle, annual_interest = annual_interest, years = new_projected_n_left/12)
        new_interest_portion <- new_principle * monthly_interest
        new_principle_portion <- new_month_payment - new_interest_portion
        new_balance <- new_principle - new_principle_portion
      }


      # if month_payment changes neet to update
      # will use specified so long as it covers interest
      if(!is.na(tmp_mp <- events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(month_payment))){
        if(tmp_mp - new_interest_portion > 0){
          new_month_payment <- tmp_mp
        } else {
          # need to cover at least the interest
          new_month_payment <- new_interest_portion + 1
        }
        #new_month_payment <- events %>% dplyr::filter(payment == new_payment) %>% dplyr::pull(month_payment)
        new_principle_portion <- new_month_payment - new_interest_portion
        new_balance <- new_principle - new_principle_portion
        new_projected_n_left <- length_mortgage(principle = new_principle, annual_interest = annual_interest, month_payment = new_month_payment)
      }
    }
    # need to exit loop if the payment doesn't cover interest otherwise it will become infinite loop
    if(new_month_payment < new_interest_portion){
      print("Payment doesn't cover interest")
      break
    }

    # update cumulative
    new_cumulative_int <- new_interest_portion + tab$cumulative_int[i]
    new_cumulative_principle <- new_principle_portion + tab$cumulative_principle[i]
    new_cumulative_total <- new_month_payment + tab$cumulative_total[i]

    # put the new row onto the table
    tab <- rbind(
      tab,
      tibble::tibble(payment = new_payment,
                     principle = new_principle,
                     interest_rate = annual_interest,
                     month_payment = new_month_payment,
                     interest_portion = new_interest_portion,
                     principle_portion = new_principle_portion,
                     balance = new_balance,
                     cumulative_int = new_cumulative_int,
                     cumulative_principle = new_cumulative_principle,
                     cumulative_total = new_cumulative_total,
                     projected_n_left = new_projected_n_left
      )
    )
    i <- i + 1

    #print(i + 1)

  }
  return(tab)
}
