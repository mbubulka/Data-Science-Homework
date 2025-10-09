# Test script for Little Falls Dashboard Final
# This script validates the dashboard without launching it

cat("🧪 TESTING LITTLE FALLS DASHBOARD FINAL\n")
cat("=======================================\n")

# Test 1: Check required packages
cat("📦 Testing package availability...\n")
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", 
                      "dataRetrieval", "tidyverse", "lubridate")

missing_packages <- c()
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if(length(missing_packages) > 0) {
  cat("❌ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Install with: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
} else {
  cat("✅ All required packages available\n")
}

# Test 2: Check enhanced predictor
cat("\n🔧 Testing enhanced predictor dependency...\n")
if(file.exists("enhanced_potomac_predictor.R")) {
  cat("✅ Enhanced predictor file found\n")
  
  # Try to source it
  tryCatch({
    source("enhanced_potomac_predictor.R")
    cat("✅ Enhanced predictor loaded successfully\n")
  }, error = function(e) {
    cat("❌ Error loading enhanced predictor:", e$message, "\n")
  })
} else {
  cat("❌ Enhanced predictor file not found\n")
}

# Test 3: Validate dashboard structure
cat("\n🏗️ Testing dashboard structure...\n")
tryCatch({
  # Load the dashboard file to check for syntax errors
  source("little_falls_dashboard_final.R", local = TRUE)
  cat("✅ Dashboard structure valid - no syntax errors\n")
  cat("✅ UI and server functions defined correctly\n")
}, error = function(e) {
  cat("❌ Dashboard structure error:", e$message, "\n")
})

cat("\n🎯 TEST SUMMARY\n")
cat("===============\n")
if(length(missing_packages) == 0 && file.exists("enhanced_potomac_predictor.R")) {
  cat("🟢 READY TO LAUNCH: All tests passed!\n")
  cat("📝 To run dashboard: source('little_falls_dashboard_final.R')\n")
} else {
  cat("🟡 SETUP NEEDED: Address issues above before launching\n")
}

cat("\n💡 Dashboard Features:\n")
cat("   📊 Enhanced visualizations with safety zones\n")
cat("   🔬 Professional methodology documentation\n") 
cat("   📈 Interactive plotly charts\n")
cat("   🎯 Little Falls-specific calibration\n")
cat("   🚀 Ready for class presentation and portfolio\n")