window.onload = function() {
    var chat = new Chat();
    chat.init();
};
var Chat = function() {
    this.socket = null;
};
Chat.prototype = {
    init: function() {

        var that = this,
        user = new Object();
        this.socket = new WebSocket("ws://" + window.location.host + "/websocket");
        this.socket.onopen = function(evt) {
            onOpen(evt)
        };
        this.socket.onmessage = function(evt) {
            onMessage(evt)
        };
        this.socket.onerror = function(evt) {
            onError(evt)
        };

        function onOpen(evt) {
            document.getElementById('messageInput').focus();
        };

        function onMessage(evt) {
            var content = JSON.parse(evt.data), color;
            for(var i = 0; i < content.length; i++){
                switch (content[i].pt) {
                case 1000:
                    user = content[i];
                    document.getElementById("nick").innerHTML = user.name;
                    break;
                case 1001:
                    color = '#000000';
                    handleMsg(content[i], color);
                case 1002:
                    color = '#000000';
                    handleMsg(content[i], color);
                    // sendAck();
                    break;
                case 1101:
                    // ack 响应
                    color = '#c00';
                    handleMsg(content[i], color);
                    break;
                default:
                    break;
                };
            };
            
            //     var msg = nickName + (type == 'login' ? ' joined' : ' left');
            //     that._displayNewMsg('system ', msg, 'red');
            //     document.getElementById('status').textContent = userCount + (userCount > 1 ? ' users' : ' user') + ' online';
        };

        function handleMsg(content, color) {
            switch (content.type) {
            case 1:
                that._displayNewMsg(content, color);
                break;
            case 2:
                that._displayImage(content, color);
                break;
            default:
                console.log("error type:" + content);
            };
        };

        function onError(evt) {
            document.getElementById('nick').textContent = '连接失败';
        };

        document.getElementById('messageInput').addEventListener('keypress',
        function(e) {

            var messageInput = document.getElementById('messageInput'),
            msg = messageInput.value;

            if (e.keyCode == 13 && msg.trim().length != 0) {
                messageInput.value = '';
                var msgObj = {
                    name: user.name,
                    type: 1,
                    msg: msg
                };
                that.socket.send(JSON.stringify(msgObj));
            };
        },
        false);

        document.getElementById('messageInput').addEventListener('keyup',
        function(e) {
            if (e.keyCode == 13) {
                document.getElementById('messageInput').value = '';
            };
        },
        false);

        document.getElementById('sendBtn').addEventListener('click',
        function() {
            var messageInput = document.getElementById('messageInput'),
            msg = messageInput.value;
            messageInput.value = '';
            messageInput.focus();
            if (msg.trim().length != 0) {
                var msgObj = {
                    name: user.name,
                    type: 1,
                    msg: msg
                };
                that.socket.send(JSON.stringify(msgObj));
                return;
            };
        },
        false);

        document.getElementById('sendImage').addEventListener('change',
        function() {
            if (this.files.length != 0) {
                var file = this.files[0],
                reader = new FileReader();
                if (!reader) {
                    alert("浏览器不支持文件读取");
                    this.value = '';
                    return;
                };
                if (file.size > 5242880) {
                    alert("图像大小大于5MB");
                    this.value = '';
                    return;
                };
                reader.onload = function(e) {
                    this.value = '';
                    var msgObj = {
                        name: user.name,
                        type: 2,
                        msg: e.target.result
                    };
                    that.socket.send(JSON.stringify(msgObj));
                };
                reader.readAsDataURL(file);
            };
        },
        false);
        this._initialEmoji();
        document.getElementById('emoji').addEventListener('click',
        function(e) {
            var emojiwrapper = document.getElementById('emojiWrapper');
            if (emojiwrapper.style.display == 'none') {
                emojiwrapper.style.display = 'block';
            } else {
                emojiwrapper.style.display = 'none';
            };
            e.stopPropagation();
        },
        false);
        document.body.addEventListener('click',
        function(e) {
            var emojiwrapper = document.getElementById('emojiWrapper');
            if (e.target != emojiwrapper) {
                emojiwrapper.style.display = 'none';
            };
        });
        document.getElementById('emojiWrapper').addEventListener('click',
        function(e) {
            var target = e.target;
            if (target.nodeName.toLowerCase() == 'img') {
                var messageInput = document.getElementById('messageInput');
                messageInput.focus();
                messageInput.value = messageInput.value + '[emoji:' + target.title + ']';
            };
        },
        false);

        document.getElementById('clearBtn').addEventListener('click',
        function() {
            document.getElementById('historyMsg').innerHTML = '';
        },
        false);
    },

    _initialEmoji: function() {
        var emojiContainer = document.getElementById('emojiWrapper'),
        docFragment = document.createDocumentFragment();
        for (var i = 69; i > 0; i--) {
            var emojiItem = document.createElement('img');
            emojiItem.src = '../static/image/tusiji/' + i + '.gif';
            emojiItem.title = i;
            docFragment.appendChild(emojiItem);
        };
        emojiContainer.appendChild(docFragment);
    },
    _displayNewMsg: function(content, color) {
        var container = document.getElementById('historyMsg'),
        msgToDisplay = document.createElement('p'),
        date = new Date(content.time).toTimeString().substr(0, 8),
        //determine whether the msg contains emoji
        msg = this._html2Escape(content.msg);
        msg = this._showEmoji(msg),
        msg = this._showMoji(msg),
        msgToDisplay.style.color = color || '#000';
        msgToDisplay.innerHTML = content.name + '<span class="timespan">(' + date + '): </span>' + msg;
        container.appendChild(msgToDisplay);

        container.scrollTop = container.scrollHeight;
    },
    _displayImage: function(content, color) {
        var container = document.getElementById('historyMsg'),
        msgToDisplay = document.createElement('p'),
        date = new Date(content.time).toTimeString().substr(0, 8);
        msgToDisplay.style.color = color || '#000';
        msgToDisplay.innerHTML = content.name + '<span class="timespan">(' + date + '): </span> <br/>' + '<a href="' + content.msg + '" target="_blank"><img src="' + content.msg + '"/></a>';
        container.appendChild(msgToDisplay);
        container.scrollTop = container.scrollHeight;
    },
    _showEmoji: function(msg) {
        var match, result = msg,
        reg = /\[emoji:\d+\]/g,
        emojiIndex, totalEmojiNum = document.getElementById('emojiWrapper').children.length;
        while (match = reg.exec(msg)) {
            emojiIndex = match[0].slice(7, -1);
            if (emojiIndex > totalEmojiNum) {
                result = result.replace(match[0], '[X]');
            } else {
                result = result.replace(match[0], '<img class="emoji" src="../static/image/tusiji/' + emojiIndex + '.gif" />');
            };
        };
        return result;
    },

    _showMoji: function(msg) {
        var FACE_MAP = [14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 15, 16, 96, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 89, 113, 114, 115, 60, 61, 46, 63, 64, 116, 66, 67, 53, 54, 55, 56, 57, 117, 59, 75, 74, 69, 49, 76, 77, 78, 79, 118, 119, 120, 121, 122, 123, 124, 42, 85, 43, 41, 86, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170];
        var FACE_WEB_MAP = [14, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 50, 51, 96, 53, 54, 73, 74, 75, 76, 77, 78, 55, 56, 57, 58, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 32, 113, 114, 115, 63, 64, 59, 33, 34, 116, 36, 37, 38, 91, 92, 93, 29, 117, 72, 45, 42, 39, 62, 46, 47, 71, 95, 118, 119, 120, 121, 122, 123, 124, 27, 21, 23, 25, 26, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170];
        var match, result = msg,
        reg = /\[face:\d+\]/g,
        faceCode, totalEmojiNum = 170;
        while (match = reg.exec(msg)) {
            faceCode = Number(match[0].slice(6, -1));
            if (faceCode > totalEmojiNum) {
                result = result.replace(match[0], '[X]');
            } else {
                var faceWebIndex = FACE_WEB_MAP.indexOf(faceCode);
                result = result.replace(match[0], '<img class="emoji" src="http://pub.idqqimg.com/lib/qqface/' + FACE_MAP[faceWebIndex] + '.gif" />');
            };
        };
        return result;
    },

    _html2Escape: function(sHtml) {
        return sHtml.replace(/[<>&"]/g,
        function(c) {
            return {
                '<': '&lt;',
                '>': '&gt;',
                '&': '&amp;',
                '"': '&quot;'
            } [c];
        });
    }
};