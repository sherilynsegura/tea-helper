# function to return tea steeping temps and times

tea <- function(tea_name) {
  tea_name <- tolower(tea_name)
  
  if (tea_name == "sencha") {
    return("Steep at 71-77 C for 1-2 mins")
  } else if (tea_name == "genmaicha") {
    return("Steep at 82-85 C for 2-3 mins")
  } else if (tea_name == "gyokuro") {
    return("Steep at 49-60 C for 2-3 mins")
  } else if (tea_name == "white peony") {
    return("Steep at 79-28 C for 2-4 mins")
  } else if (tea_name == "oolong") {
    return("Steep at 82-96 C for 2-5 mins")
  } else if (tea_name == "assam") {
    return("Steep at 93-100 C for 3-5 mins")
  } else if (tea_name == "darjeeling") {
    return("Steep at 85-91 C for 2-3 mins")
  } else if (tea_name == "earl gray" || tea_name == "earl grey") {
    return("Steep at 93-100 C for 3-5 mins")
  } else if (tea_name == "english breakfast") {
    return("Steep at 93-100 C for 3-5 mins")
  } else if (tea_name == "black tea") {
    return("Steep at 93-100 C for 3-5 mins")
  } else if (tea_name == "chamomile") {
    return("Steep at 100 C for 5-10 mins")
  } else if (tea_name == "peppermint") {
    return("Steep at 100 C for 5-8 mins")
  } else if (tea_name == "rooibos") {
    return("Steep at 100 C for 5-7 mins")
  } else if (tea_name == "hibiscus") {
    return("Steep at 100 C for 5-10 mins")
  } else if (tea_name == "herbal") {
    return("Steep at 100 C for 5-7 mins")
  } else if (tea_name == "white tea") {
    return("Steep at 71-79 C for 1-3 mins")
  } else if (tea_name == "green tea") {
    return("Steep at 70-82 C for 1-3 mins")
  } else if (tea_name == "ginger") {
    return("Steep at 100 C for 5-10 mins (bag) or 10-20 mins (fresh)")
  } else {
    return("Tea not recognized. Please check the tea name.")
  }
}

# test
tea("earl gray") #"Steep at 93-100 C for 3-5 mins"
tea("Chamomile") #"Steep at 100 C for 5-10 mins"
tea("ROOIBOS") #"Steep at 100 C for 5-7 mins"
tea("ginger") #"Steep at 100 C for 5-10 mins (bag) or 10-20 mins (fresh)"



# function to recommend tea based on primary concern

recommend_tea <- function(symptom) {
  symptom <- tolower(trimws(symptom))
  
  if (symptom == "insomnia" || symptom == "trouble sleeping") {
    return("chamomile")
  } else if (symptom == "nausea" || symptom == "upset stomach") {
    return("ginger")
  } else if (symptom == "bloating" || symptom == "gas") {
    return("peppermint")
  } else if (symptom == "anxiety" || symptom == "anxious") {
    return("chamomile")
  } else if (symptom == "congestion") {
    return("peppermint")
  } else if (symptom == "low energy") {
    return("sencha")
  } else if (symptom == "stress" || symptom == "poor focus") {
    return("gyokuro")
  } else if (symptom == "brain fog") {
    return("green tea")
  } else if (symptom == "fatigue") {
    return("assam (black teas)")
  } else if (symptom == "low mood") {
    return("earl grey")
  } else if (symptom == "cramps" || symptom == "cramping") {
    return("chamomile")
  } else if (symptom == "acid reflux") {
    return("rooibos")
  } else {
    return(paste(
      "No tea suggestion available for that symptom.",
      "Please enter one of these options:",
      "insomnia, trouble sleeping, nausea, upset stomach,",
      "bloating, gas, anxiety, anxious, congestion, low energy,",
      "stress, poor focus, brain fog, fatigue, low mood,",
      "cramps, cramping, acid reflux."
    ))
  }
}

recommend_tea("fatigue") #"assam (black teas)"
recommend_tea("cramping") #"chamomile"
recommend_tea("tired") #"No tea suggestion available for that symptom. Please 
                       #enter one of these options: insomnia, trouble sleeping, 
                       #nausea, upset stomach, bloating, gas, anxiety, anxious, 
                       #congestion, low energy, stress, poor focus, brain fog, fatigue, 
                       #low mood, cramps, cramping, acid reflux."
