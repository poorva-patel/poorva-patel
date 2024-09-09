# Use  Python 
FROM python:3.9-slim

# Set the working directory in the container
WORKDIR /app

# Install necessary system dependencies
RUN apt-get update && apt-get install -y python3-distutils
#RUN pip install --no-cache-dir -r requirements.txt

# Copy the current directory contents into the container 
COPY . /app

# Install any needed packages in requirements.txt
RUN pip install --no-cache-dir -r requirements.txt

# Make port 8000 available 
EXPOSE 8000

# Run the FastAPI application
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
