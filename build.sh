#!/bin/bash

# Force remove existing build directory for a clean build
echo "Removing previous build directory..."
rm -rf build

# Create build directory
echo "Creating build directory..."
mkdir -p build

# Enter build directory
cd build

# Configure CMake
echo "Configuring CMake..."
cmake ..

# Build the project
echo "Building project..."
cmake --build .

# Go back to the parent directory (optional, good practice)
cd ..

echo "Build complete. The executable is in the build directory."

