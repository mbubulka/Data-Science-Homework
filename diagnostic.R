# Diagnostic Script - Check R Environment
print("=== R Environment Diagnostic ===")
print(paste("R Version:", R.version.string))
print(paste("Working Directory:", getwd()))

# Check if required packages are available
required_packages <- c("ggplot2", "dplyr", "tidyr")

print("\n=== Package Check ===")
for(pkg in required_packages) {
  if(require(pkg, character.only = TRUE, quietly = TRUE)) {
    print(paste("✓", pkg, "- AVAILABLE"))
  } else {
    print(paste("✗", pkg, "- MISSING - Installing..."))
    install.packages(pkg)
    if(require(pkg, character.only = TRUE, quietly = TRUE)) {
      print(paste("✓", pkg, "- INSTALLED SUCCESSFULLY"))
    } else {
      print(paste("✗", pkg, "- INSTALLATION FAILED"))
    }
  }
}

# Test basic ggplot functionality
print("\n=== Basic ggplot2 Test ===")
tryCatch({
  library(ggplot2)
  test_plot <- ggplot(data.frame(x=1:3, y=1:3), aes(x, y)) + geom_point()
  print("✓ Basic ggplot2 works")
  
  # Test ggsave
  ggsave("test_plot.png", plot = test_plot, width = 6, height = 4)
  if(file.exists("test_plot.png")) {
    print("✓ ggsave works - test_plot.png created")
    file.remove("test_plot.png")  # Clean up
  } else {
    print("✗ ggsave failed - no file created")
  }
  
}, error = function(e) {
  print(paste("✗ ggplot2 test failed:", e$message))
})

print("\n=== Diagnostic Complete ===")
print("If all tests pass, your R environment should work fine.")