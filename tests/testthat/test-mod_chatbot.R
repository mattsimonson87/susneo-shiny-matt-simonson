test_that("chatbot module loads without error when API key present", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "No OpenAI API key set")
  
  # Test that the module functions exist
  expect_true(exists("mod_chatbot_ui"))
  expect_true(exists("mod_chatbot_server"))
})

test_that("chatbot UI renders", {
  # Test UI creation (doesn't need API key)
  ui <- mod_chatbot_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})