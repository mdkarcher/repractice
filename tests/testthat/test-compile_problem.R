test_that("string interpolation works", {
  text="The <<adjective>> <<plnoun>> are <<gerund>> <<adverb>>."
  vars=list(adjective="blue", plnoun="beetles", gerund="scurrying", adverb="merrily")
  expect_equal(
    compile_text(text = text, vars=vars),
    "The blue beetles are scurrying merrily."
  )
})

