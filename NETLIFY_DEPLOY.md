# 🚀 Netlify Deployment Guide for Potomac Dashboard

## Step 1: Deploy to Netlify

1. **Go to [Netlify.com](https://netlify.com)**
2. **Drag and drop** the `potomac_static.html` file onto the deployment area
3. **Netlify will give you a URL** like: `https://wonderful-dashboard-abc123.netlify.app`
4. **Optional**: Change the site name to something like `potomac-safety-dashboard`

## Step 2: Integration with bubulkaanalytics.com

### For your Analytics Dashboard section, use this iframe code:

```html
<div class="dashboard-embed" style="width: 100%; margin: 20px 0;">
    <h3>🌊 Potomac River Safety Analysis</h3>
    <p class="text-muted">Real-time kayaking conditions and 7-day forecast</p>
    
    <iframe 
        src="https://your-netlify-url.netlify.app"
        width="100%" 
        height="800"
        frameborder="0"
        scrolling="auto"
        style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);">
        <p>Your browser doesn't support iframes. <a href="https://your-netlify-url.netlify.app">View dashboard directly</a></p>
    </iframe>
</div>
```

### Alternative: Responsive iframe with auto-height

```html
<div class="dashboard-embed">
    <h3>🌊 Potomac River Safety Analysis</h3>
    <iframe 
        id="potomac-dashboard"
        src="https://your-netlify-url.netlify.app"
        width="100%" 
        height="800"
        frameborder="0"
        style="border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);">
    </iframe>
    
    <script>
        // Auto-resize iframe based on content
        window.addEventListener('message', function(event) {
            if (event.data.type === 'iframe-height' && event.data.source === 'potomac-dashboard') {
                const iframe = document.getElementById('potomac-dashboard');
                if (iframe) {
                    iframe.style.height = (event.data.height + 50) + 'px';
                }
            }
        });
    </script>
</div>
```

## Step 3: Features for bubulkaanalytics.com

✅ **Professional Integration**: Matches your portfolio theme  
✅ **Responsive Design**: Works on all devices  
✅ **Auto-refresh**: Updates every 5 minutes  
✅ **Real-time Data**: Uses USGS API when available  
✅ **Fallback System**: Demo mode if API fails  
✅ **Performance**: Lightweight and fast loading  
✅ **Iframe Optimized**: Detects embedding and adjusts layout  

## Step 4: Optional Custom Domain

If you want: `potomac.bubulkaanalytics.com`

1. **In Netlify**: Go to Site Settings → Domain Management
2. **Add custom domain**: `potomac.bubulkaanalytics.com`
3. **Update DNS**: Add CNAME record pointing to your Netlify URL
4. **SSL**: Netlify handles HTTPS automatically

## Troubleshooting

- **If iframe doesn't show**: Check CORS settings (Netlify handles this)
- **If height is wrong**: The auto-resize script will fix it
- **If styling breaks**: The dashboard detects iframe embedding automatically

## File Structure
```
deploy/
├── potomac_static.html    (Main dashboard - upload this to Netlify)
├── potomac_dashboard.Rmd  (R Markdown version for GitHub Pages)
├── custom.css            (CSS for R Markdown version)
└── NETLIFY_DEPLOY.md     (This file)
```

Ready to deploy! 🎉