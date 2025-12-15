# Deployment Checklist for shinyapps.io

Follow these steps to deploy your Synthetic Control Analysis app to shinyapps.io.

---

## Prerequisites

1. ✅ R and RStudio installed
2. ✅ A shinyapps.io account (free tier is fine)
3. ✅ Internet connection

---

## Step 1: Install Required Packages

Open R or RStudio and run:

```r
# Install deployment package
install.packages("rsconnect")

# Install app dependencies (if not already installed)
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "readxl",
  "dplyr",
  "tidyr",
  "Synth",
  "zip"
))
```

**Wait for all packages to install successfully before proceeding.**

---

## Step 2: Get Your shinyapps.io Credentials

1. Go to https://www.shinyapps.io/
2. Log in to your account
3. Click your **name** (top right corner)
4. Click **"Tokens"** in the dropdown
5. Click **"Show"** button
6. Click **"Show Secret"** button
7. **Copy** the three values:
   - Account name
   - Token
   - Secret

---

## Step 3: Configure deploy.R Script

1. Open `deploy.R` in RStudio or a text editor
2. Replace these placeholders with your actual credentials:
   ```r
   name   = "YOUR_ACCOUNT_NAME",    # Replace with your account name
   token  = "YOUR_TOKEN_HERE",      # Replace with your token
   secret = "YOUR_SECRET_HERE"      # Replace with your secret
   ```
3. **Save the file** (keep credentials private!)

---

## Step 4: Deploy the App

### Option A: Run deploy.R Script (Recommended)

In RStudio:
1. Open `deploy.R`
2. Click **"Source"** (or press Cmd/Ctrl+Shift+S)
3. Wait for deployment to complete (may take 2-5 minutes)
4. Your browser will open the deployed app automatically

### Option B: Manual Deployment

In R Console:
```r
setwd("/Users/hkhkowl/shinyR/cs130-synthetic-control-shiny-app")
source("deploy.R")
```

---

## Step 5: Verify Deployment

Once deployment completes, you should see:

✅ Success message: "Application successfully deployed to..."
✅ Browser opens with your app at: `https://YOUR_ACCOUNT.shinyapps.io/synthetic-control-analysis/`
✅ App loads and is functional

### Test the deployed app:
1. Upload a dataset (try `california_prop99.csv`)
2. Configure an analysis
3. Run the analysis
4. Check that results display correctly

---

## Troubleshooting

### Error: "Package installation failed"

**Solution**: Install missing packages locally first:
```r
install.packages("PACKAGE_NAME")
```

### Error: "Authorization failed"

**Solution**:
1. Double-check your credentials in `deploy.R`
2. Regenerate token at shinyapps.io if needed
3. Make sure there are no extra spaces in credentials

### Error: "Deployment timeout"

**Solution**:
1. Check your internet connection
2. Try again (sometimes servers are busy)
3. Use `forceUpdate = TRUE` in deployApp()

### App deployed but not working

**Solution**:
1. Check logs: `rsconnect::showLogs(appName = "synthetic-control-analysis")`
2. Look for error messages
3. Test locally first: `shiny::runApp("/Users/hkhkowl/shinyR/cs130-synthetic-control-shiny-app")`

---

## Re-Deploying After Changes

After making changes to your app:

```r
setwd("/Users/hkhkowl/shinyR/cs130-synthetic-control-shiny-app")
rsconnect::deployApp(
  appDir = ".",
  appName = "synthetic-control-analysis",
  forceUpdate = TRUE
)
```

---

## Managing Your Deployed App

### View Logs
```r
rsconnect::showLogs(appName = "synthetic-control-analysis")
```

### Check Deployed Apps
```r
rsconnect::applications()
```

### Terminate App
```r
rsconnect::terminateApp(appName = "synthetic-control-analysis")
```

### Restart App
Log in to shinyapps.io dashboard and click "Restart" button.

---

## Free Tier Limits (shinyapps.io)

- **5 applications** maximum
- **25 active hours** per month
- **1 GB memory** per app
- App sleeps after 15 minutes of inactivity

**Tip**: For classroom use or demos, consider upgrading to a paid plan or using your institutional server.

---

## Security Notes

⚠️ **NEVER commit `deploy.R` with real credentials to git!**

To protect your credentials:

1. Add to `.gitignore`:
   ```
   deploy.R
   rsconnect/
   ```

2. Or create a separate `deploy_credentials.R`:
   ```r
   # deploy_credentials.R (add to .gitignore)
   ACCOUNT_NAME <- "your_name"
   ACCOUNT_TOKEN <- "your_token"
   ACCOUNT_SECRET <- "your_secret"
   ```

   Then in `deploy.R`:
   ```r
   source("deploy_credentials.R")
   rsconnect::setAccountInfo(
     name = ACCOUNT_NAME,
     token = ACCOUNT_TOKEN,
     secret = ACCOUNT_SECRET
   )
   ```

---

## Share Your App

Once deployed, share this URL:
```
https://YOUR_ACCOUNT_NAME.shinyapps.io/synthetic-control-analysis/
```

**Example**: `https://johnsmith.shinyapps.io/synthetic-control-analysis/`

---

## Questions?

- **shinyapps.io Documentation**: https://docs.posit.co/shinyapps.io/
- **rsconnect Package**: https://rstudio.github.io/rsconnect/
- **Shiny Deployment Guide**: https://shiny.posit.co/r/articles/share/shinyapps/

---

**Good luck with your deployment! 🚀**

*Created by Hakkei Sekine & Maria Dumitrescu, 2025*
