ACCESS_TOKEN=58958865db704923926fc8a3561ff05a
ENVIRONMENT=production
LOCAL_USERNAME=`whoami`
REVISION=`git log -n 1 --pretty=format:"%H"`

./build.sh && \
  cd dist && \
  git add -A . && \
  git commit && \
  git push origin gh-pages && \
  curl https://api.rollbar.com/api/1/deploy/ \
    -F access_token=$ACCESS_TOKEN \
    -F environment=$ENVIRONMENT \
    -F revision=$REVISION \
    -F local_username=$LOCAL_USERNAME

