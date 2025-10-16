# Dockerfile for CollabCanvas with Woo
# Based on Fukamachi's battle-tested template for production Lisp applications

FROM fukamachi/sbcl:2.4.9

# Set environment variables
ENV PORT=5000 \
    QUICKLISP_ADD_TO_INIT_FILE=true \
    CL_SOURCE_REGISTRY="/app:$CL_SOURCE_REGISTRY" \
    DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    # Build essentials
    gcc \
    libc6-dev \
    make \
    # Libev for Woo's event loop
    libev-dev \
    # SQLite
    libsqlite3-dev \
    sqlite3 \
    # Networking tools for debugging
    curl \
    netcat-openbsd \
    # Clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy entrypoint script
COPY entrypoint.sh /srv/entrypoint.sh
RUN chmod +x /srv/entrypoint.sh

# Copy application code
COPY . /app/

# Install Roswell dependencies
# Note: Quicklisp is pre-installed in fukamachi/sbcl image
RUN ros install clack woo

# Load and compile application to cache compilation
RUN sbcl --non-interactive \
    --eval '(ql:quickload :collabcanvas)' \
    --eval '(format t "~%✓ CollabCanvas loaded successfully~%")'

# Create data directory for SQLite database
RUN mkdir -p /app/data

# Expose port
EXPOSE 5000

# Set entrypoint and default command
ENTRYPOINT ["/srv/entrypoint.sh"]
CMD ["start"]
