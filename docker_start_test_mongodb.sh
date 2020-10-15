docker stop emdash-test-db
docker rm emdash-test-db
docker run -d -p 27017:27017 -v $(pwd)/test-data:/root --name emdash-test-db mongo:latest
docker exec emdash-test-db sh -c 'cd ~ && mongorestore'
