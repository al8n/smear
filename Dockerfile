FROM cimg/rust:1.89.0-node

# Set working directory
WORKDIR /app

# Install minimal dependencies for common Rust projects
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy your project files
COPY . .

# Build dependencies (this layer will be cached if Cargo.toml doesn't change)
RUN cargo build --tests

# Run tests by default
CMD ["cargo", "test"]