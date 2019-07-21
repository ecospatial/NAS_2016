# Install docker
sudo yum install -y docker
sudo service docker start
sudo usermod -a -G docker ec2-user

# Upgrade Setuptools
sudo pip install setuptools --upgrade

# Install OpenSSL Development package
sudo yum install -y openssl-devel 

# Install pyasn1 dependency
sudo pip install --yes --upgrade pyasn1-modules

# Install Google EE Python API
sudo pip install --yes earthengine-api

# Install script package dependencies
sudo pip install pandas

# Run docker container
############# THIS ISNT AUTOMATED BY THE SCRIPT FOR SOME REASON ###############
export GCP_PROJECT_ID=NAS_2016
export CONTAINER_IMAGE_NAME=gcr.io/earthengine-project/datalab-ee:latest
export WORKSPACE=${HOME}/workspace/datalab-ee
mkdir -p $WORKSPACE
cd $WORKSPACE

docker run -it -p 8081:8080 -v "$WORKSPACE:/content" -e "PROJECT_ID=$GCP_PROJECT_ID" $CONTAINER_IMAGE_NAME

# Auth Code: 4/AACO5l15GHOxuOYxcemqQBmPNUHoN4EPkhwXw9VQWhUCY2HZXXCft30