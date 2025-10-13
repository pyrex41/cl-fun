# Dockerfile for CollabCanvas

FROM debian:bookworm-slim

# Install system dependencies
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

# Setup Roswell
RUN ros setup

# Install SBCL via Roswell
RUN ros install sbcl-bin

# Set working directory
WORKDIR /app

# Copy backend files
COPY backend/ ./backend/

# Setup backend
WORKDIR /app/backend
RUN ros -e '(ql:quickload :quicklisp-slime-helper)' -q

# Link project to Roswell
RUN ln -s /app/backend ~/.roswell/local-projects/collabcanvas && \
    ros -e '(ql:register-local-projects)' -q

# Copy frontend files
WORKDIR /app
COPY frontend/ ./frontend/

# Build frontend
WORKDIR /app/frontend
RUN npm install && npm run build

# Create data directory
RUN mkdir -p /app/backend/data

# Expose ports
EXPOSE 8080 5173

# Set working directory
WORKDIR /app

# Create startup script
RUN echo '#!/bin/bash\n\
cd /app/backend\n\
sqlite3 data/canvas.db < db/schema.sql\n\
ros run -e "(ql:quickload :collabcanvas :silent t)" \\\n\
        -e "(collabcanvas:start-server)" \\\n\
        -e "(loop (sleep 1))" -q' > /app/start.sh && \
    chmod +x /app/start.sh

# Start the application
CMD ["/app/start.sh"]