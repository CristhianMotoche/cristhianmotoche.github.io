DATE=$(date +'%F')

echo "*[NEW] Insert the title here: "
read title

read -r -d '' TEXT << EOM
---
title: ${title}
date: $(date +'%F %X')
tags:
description:
---
EOM

if [ -z "$title" ]; then
    echo "[ERROR]: Need to set a title"
    exit 1
fi

echo "${TEXT}" > "posts/$DATE-$title.md"
