#!/bin/bash
set -eu

# -----------------------------------------------------------------------
# Test using OpenAL API using curl.
# *THIS IS NOT A SCRIPT TO BE RUN AS-IS*,
# you need to understand it and uncomment each part in turn -- I was too lazy
# to try to process JSONs using script.
#
# Refs:
# https://platform.openai.com/docs/api-reference/messages/listMessages
# v2: https://platform.openai.com/docs/assistants/migration
# docs: https://platform.openai.com/docs/assistants/overview?context=without-streaming
# -----------------------------------------------------------------------

# Fill this with your rel API key and assistant id.
OPENAI_API_KEY='...'
OPENAI_ASSISTANT_ID='...'

# curl https://api.openai.com/v1/threads \
#   -H "Content-Type: application/json" \
#   -H "Authorization: Bearer $OPENAI_API_KEY" \
#   -H "OpenAI-Beta: assistants=v2" \
#   -d ''

# Get id from JSON response above
THREAD_ID='thread_...'

# curl https://api.openai.com/v1/threads/${THREAD_ID}/messages \
#   -H "Content-Type: application/json" \
#   -H "Authorization: Bearer $OPENAI_API_KEY" \
#   -H "OpenAI-Beta: assistants=v2" \
#   -d '{
#       "role": "user",
#       "content": "What is 2 + 2, add a cat joke."
#     }'

MESSAGE_ID='msg_...'

# curl https://api.openai.com/v1/threads/${THREAD_ID}/runs \
#   -H "Authorization: Bearer $OPENAI_API_KEY" \
#   -H "Content-Type: application/json" \
#   -H "OpenAI-Beta: assistants=v2" \
#   -d '{
#     "assistant_id": "'${OPENAI_ASSISTANT_ID}'"
#   }'

RUN_ID='run_...'

# curl https://api.openai.com/v1/threads/${THREAD_ID}/runs/${RUN_ID} \
#   -H "Authorization: Bearer $OPENAI_API_KEY" \
#   -H "OpenAI-Beta: assistants=v2"

# The above needs to be repeated until the run has status: completed.
# Then list messages:

# curl https://api.openai.com/v1/threads/${THREAD_ID}/messages?limit=1 \
#   -H "Content-Type: application/json" \
#   -H "Authorization: Bearer $OPENAI_API_KEY" \
#   -H "OpenAI-Beta: assistants=v2"

# The last answer is in 1st item of above.