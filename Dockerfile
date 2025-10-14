# Multi-stage Dockerfile for CollabCanvas
# Stage 1: Build and compile everything, create standalone binary

FROM debian:bookworm-slim AS builder

# Install system dependencies for building
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libssl-dev \
    sqlite3 \
    libsqlite3-dev \
    nodejs \
    npm \
    && rm -rf /var/lib/apt/lists/*

# Install Roswell
RUN curl -L https://github.com/roswell/roswell/releases/download/v21.10.14.111/roswell_21.10.14.111-1_amd64.deb \
    -o roswell.deb && \
    dpkg -i roswell.deb && \
    rm roswell.deb

# Setup Roswell and install SBCL
RUN ros setup && ros install sbcl-bin

# Set working directory
WORKDIR /app

# Copy and compile backend
COPY backend/ ./backend/
WORKDIR /app/backend

# Link project to Roswell and compile all dependencies
RUN ln -s /app/backend ~/.roswell/local-projects/collabcanvas && \
    ros -e '(ql:register-local-projects)' -q && \
    ros -e '(ql:quickload :collabcanvas)' -q && \
    echo "Dependencies compiled successfully"

# Create standalone binary with embedded Lisp runtime
# First ensure the system loads cleanly
RUN ros -e '(ql:quickload :collabcanvas)' && \
    echo "System loaded successfully" && \
    ros -e '(ql:quickload :collabcanvas)' \
        -e '(sb-ext:save-lisp-and-die "collabcanvas-server" \
              :toplevel (function collabcanvas:main) \
              :executable t \
              :compression t \
              :save-runtime-options t)' && \
    test -f collabcanvas-server && \
    chmod +x collabcanvas-server && \
    echo "Standalone binary created successfully" && \
    ls -lh collabcanvas-server

# Build frontend
WORKDIR /app/frontend
COPY frontend/package*.json ./
RUN npm install
COPY frontend/ ./
RUN npm run build

# Stage 2: Minimal runtime image with just the binary and assets

FROM debian:bookworm-slim

# Install only runtime dependencies (SQLite for database)
RUN apt-get update && apt-get install -y \
    sqlite3 \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy standalone binary from builder
COPY --from=builder /app/backend/collabcanvas-server /app/collabcanvas-server

# Copy frontend build from builder
COPY --from=builder /app/frontend/dist /app/frontend/dist

# Copy database schema
COPY --from=builder /app/backend/db /app/backend/db

# Create data directory for volume mount
RUN mkdir -p /data

# Expose port
EXPOSE 8080

# Create startup script - just initialize DB and run binary
RUN echo '#!/bin/bash\n\
set -e\n\
\n\
# Check if database exists, if not initialize it\n\
if [ ! -f /data/canvas.db ]; then\n\
  echo "Initializing database..."\n\
  sqlite3 /data/canvas.db < /app/backend/db/schema.sql\n\
  echo "Database initialized."\n\
else\n\
  echo "Database already exists, skipping initialization."\n\
fi\n\
\n\
# Start the server (binary already compiled, should start in <5 seconds)\n\
echo "Starting CollabCanvas server..."\n\
exec /app/collabcanvas-server' > /app/start.sh && \
    chmod +x /app/start.sh

# Start the application
CMD ["/app/start.sh"]
