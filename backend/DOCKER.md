# Docker Deployment Guide for CollabCanvas

This guide explains how to build, run, and deploy CollabCanvas using Docker and Docker Compose.

## Overview

CollabCanvas uses a production-ready Docker setup based on Fukamachi's battle-tested template for Common Lisp applications. The container includes:

- **Base Image**: `fukamachi/sbcl:2.4.9`
- **Web Server**: Woo (high-performance async HTTP/WebSocket server)
- **Framework**: Clack (platform-agnostic web framework)
- **Database**: SQLite (file-based, persisted in volume)
- **Port**: 5000 (configurable via `PORT` environment variable)

## Prerequisites

- Docker 20.10+
- Docker Compose 1.29+ (optional, for local testing)

## Quick Start

### Using Docker Compose (Recommended for Local Development)

```bash
# Build and start the application
docker-compose up -d

# View logs
docker-compose logs -f

# Stop the application
docker-compose down

# Rebuild after code changes
docker-compose up -d --build
```

The application will be available at:
- **HTTP API**: http://localhost:5000/api
- **WebSocket**: ws://localhost:5000/ws/<canvas-id>
- **Health Check**: http://localhost:5000/health

### Using Docker CLI

```bash
# Build the image
docker build -t collabcanvas:latest .

# Run the container
docker run -d \
  --name collabcanvas \
  -p 5000:5000 \
  -v $(pwd)/data:/app/data \
  collabcanvas:latest

# View logs
docker logs -f collabcanvas

# Stop the container
docker stop collabcanvas
docker rm collabcanvas
```

## Configuration

### Environment Variables

Configure the application using environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `5000` | Server port |
| `CORS_ORIGIN` | `*` | CORS allowed origin (set to specific domain in production) |
| `SESSION_TIMEOUT` | `86400` | Session timeout in seconds (24 hours) |
| `DEBUG` | `false` | Enable debug mode |

Example:

```bash
docker run -d \
  -p 5000:5000 \
  -e PORT=8080 \
  -e CORS_ORIGIN=https://yourfrontend.com \
  -e SESSION_TIMEOUT=3600 \
  collabcanvas:latest
```

### Persistent Data

The SQLite database is stored in `/app/data/canvas.db` inside the container. To persist data across container restarts, mount a volume:

```bash
-v $(pwd)/data:/app/data
```

Or in docker-compose.yml:

```yaml
volumes:
  - ./data:/app/data
```

## Database Initialization

The database is automatically initialized on first run using the schema file at `/app/db/schema.sql`. If the database file doesn't exist, the entrypoint script will:

1. Check for `schema.sql`
2. Create the database
3. Execute the schema to set up tables

## Entrypoint Commands

The entrypoint script supports multiple commands:

### Start Server (Default)

```bash
docker run collabcanvas:latest start
# or simply
docker run collabcanvas:latest
```

### Interactive REPL

```bash
docker run -it collabcanvas:latest repl
```

### Run Tests

```bash
docker run collabcanvas:latest test
```

### Shell Access

```bash
docker run -it collabcanvas:latest shell
```

## Health Checks

The container includes a health check that pings the `/health` endpoint every 30 seconds:

```bash
# Check container health
docker ps

# View health check logs
docker inspect collabcanvas | jq '.[0].State.Health'
```

Expected health response:

```json
{
  "status": "ok",
  "service": "collabcanvas",
  "timestamp": 3913891200
}
```

## Building for Production

### Multi-Stage Build (Recommended)

For production deployments, use a multi-stage build to minimize image size:

```dockerfile
# Stage 1: Build frontend
FROM node:20 AS frontend-builder
WORKDIR /frontend
COPY frontend/package*.json ./
RUN npm ci --only=production
COPY frontend/ ./
RUN npm run build

# Stage 2: Production image
FROM fukamachi/sbcl:2.4.9
# ... copy frontend dist from frontend-builder stage
```

### Resource Limits

The docker-compose.yml includes sensible resource limits:

```yaml
deploy:
  resources:
    limits:
      cpus: '2.0'
      memory: 2G
    reservations:
      cpus: '0.5'
      memory: 512M
```

Adjust based on your expected load.

## Performance Tuning

### Woo Server Configuration

The server is configured for production in `src/server.lisp`:

- **Debug mode**: `nil` (disabled)
- **Worker threads**: `1` (SQLite requires single-writer)
- **Socket backlog**: `4096`
- **Event loop**: Pure async (`use-thread: nil`)

### Monitoring

The application logs connection statistics every 60 seconds:

```
[STATS] ((:TIMESTAMP . 3913891200) (:TOTAL-CONNECTIONS . 42) (:TOTAL-ROOMS . 5) (:MEMORY-MB . 256) (:THREADS . 8))
```

Monitor these metrics to track performance.

## Troubleshooting

### Container Won't Start

Check logs for errors:

```bash
docker logs collabcanvas
```

Common issues:
- Port already in use: Change `PORT` environment variable
- Database initialization failed: Check `/app/db/schema.sql` exists
- Out of memory: Increase memory limits

### WebSocket Connections Fail

Ensure WebSocket upgrade headers are properly forwarded if behind a reverse proxy:

```nginx
# Nginx example
location /ws/ {
    proxy_pass http://collabcanvas:5000;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
}
```

### Database Locked Errors

SQLite requires single-writer access. Ensure `worker-num: 1` in server configuration. For multi-instance deployments, migrate to PostgreSQL.

## Deployment Platforms

### Fly.io

```bash
# Install flyctl
curl -L https://fly.io/install.sh | sh

# Login
flyctl auth login

# Initialize app
flyctl launch

# Deploy
flyctl deploy
```

### Railway

```bash
# Install Railway CLI
npm i -g @railway/cli

# Login
railway login

# Initialize project
railway init

# Deploy
railway up
```

### Kubernetes

See `k8s/` directory for Kubernetes manifests (deployment, service, ingress).

## Security Considerations

1. **CORS**: Set `CORS_ORIGIN` to specific domains in production
2. **HTTPS**: Use reverse proxy (Nginx, Caddy) with TLS termination
3. **Secrets**: Never commit database files or .env files
4. **Updates**: Keep base image updated (`fukamachi/sbcl:latest`)
5. **Non-root**: Container runs as root by default; consider adding user

## Next Steps

1. Set up CI/CD pipeline for automated builds
2. Configure monitoring with Prometheus/Grafana
3. Implement log aggregation (ELK stack, Datadog)
4. Set up automated backups for SQLite database
5. Load test with `wrk` (HTTP) and `thor` (WebSocket)

## References

- [Woo Documentation](https://github.com/fukamachi/woo)
- [Clack Documentation](https://github.com/fukamachi/clack)
- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Fukamachi's SBCL Image](https://hub.docker.com/r/fukamachi/sbcl)
