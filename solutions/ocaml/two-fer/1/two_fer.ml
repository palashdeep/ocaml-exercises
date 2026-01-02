let two_fer s = 
  let who = Option.value s ~default:"you" in
  "One for " ^ who ^ ", one for me."
