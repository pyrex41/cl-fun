# Deployment Guide for CollabCanvas on Fly.io

## Prerequisites Checklist

- [ ] Fly.io CLI installed (`brew install flyctl`)
- [ ] Logged into Fly.io (`fly auth login`)
- [ ] Auth0 account set up with application credentials
- [ ] Auth0 callback URL configured: `https://cl-fun.fly.dev/auth0/callback`

## Step 1: Set Auth0 Secrets

These secrets are required for OAuth to work in production:

```bash
# Get these values from your Auth0 dashboard: https://manage.auth0.com/
fly secrets set AUTH0_DOMAIN=your-tenant.us.auth0.com
fly secrets set AUTH0_CLIENT_ID=your_client_id_here
fly secrets set AUTH0_CLIENT_SECRET=your_client_secret_here
fly secrets set AUTH0_CALLBACK_URL=https://cl-fun.fly.dev/auth0/callback
```

## Step 2: Configure Auth0 Dashboard

In your Auth0 application settings:

1. **Allowed Callback URLs**: Add `https://cl-fun.fly.dev/auth0/callback`
2. **Allowed Logout URLs**: Add `https://cl-fun.fly.dev`
3. **Allowed Web Origins**: Add `https://cl-fun.fly.dev`
4. **Application Type**: Regular Web Application

## Step 3: Deploy to Fly.io

```bash
# Deploy the application
fly deploy

# Check deployment status
fly status

# View logs
fly logs
```

## Step 4: Verify Deployment

1. Open `https://cl-fun.fly.dev`
2. Try logging in with Auth0
3. Draw an object and verify persistence
4. Check color preference persistence

## Database Migrations

The startup script automatically handles migrations:
- ✅ Auth0 columns (auth0_sub, display_name, etc.)
- ✅ Color preference column (preferred_color)
- ✅ Indexes for Auth0 lookups

Migrations run idempotently on every startup, so they're safe to run multiple times.

## Troubleshooting

### Check Logs
```bash
fly logs --app cl-fun
```

### SSH into Container
```bash
fly ssh console --app cl-fun
```

### Check Database
```bash
fly ssh console --app cl-fun
sqlite3 /data/canvas.db ".schema users"
```

### Restart App
```bash
fly apps restart cl-fun
```

## Environment Variables

### Set in fly.toml
- `DATABASE_PATH=/data/canvas.db`
- `FRONTEND_PATH=/app/frontend/dist/`
- `CORS_ORIGIN=https://cl-fun.fly.dev`

### Set as Secrets (via CLI)
- `AUTH0_DOMAIN`
- `AUTH0_CLIENT_ID`
- `AUTH0_CLIENT_SECRET`
- `AUTH0_CALLBACK_URL`

## Health Checks

The app exposes `/health` endpoint:
```bash
curl https://cl-fun.fly.dev/health
```

Expected response:
```json
{
  "status": "ok",
  "service": "collabcanvas",
  "timestamp": 1234567890
}
```

## Scaling

Current configuration:
- Memory: 1GB
- CPU: 1 shared vCPU
- Auto-stop: Enabled (suspends when idle)
- Auto-start: Enabled
- Min machines: 0

To scale up:
```bash
fly scale memory 2048  # 2GB RAM
fly scale count 2      # 2 instances
```

## Rollback

If deployment fails:
```bash
fly releases               # List releases
fly releases rollback     # Rollback to previous
```
