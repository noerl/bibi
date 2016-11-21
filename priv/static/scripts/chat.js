window.onload = function() {
    var chat = new Chat();
    chat.init();
};
var Chat = function() {
    this.socket = null;
};
Chat.prototype = {
    init: function() {
        var that = this, name = "";
        this.socket = new WebSocket("ws://" + window.location.host + "/websocket"); 
        this.socket.onopen = function(evt) { onOpen(evt) }; 
        this.socket.onmessage = function(evt) { onMessage(evt) }; 
        this.socket.onerror = function(evt) { onError(evt) };

        function onOpen(evt) {
            document.getElementById('messageInput').focus();
        };  

        function onMessage(evt) {
            user = 'system'
            color = '#000000';
            that._displayNewMsg(user, evt.data, color);
        //     var msg = nickName + (type == 'login' ? ' joined' : ' left');
        //     that._displayNewMsg('system ', msg, 'red');
        //     document.getElementById('status').textContent = userCount + (userCount > 1 ? ' users' : ' user') + ' online';

        // this.socket.on('newMsg', function(user, msg, color) {
        //     that._displayNewMsg(user, msg, color);
        // });
        // this.socket.on('newImg', function(user, img, color) {
        //     that._displayImage(user, img, color);
        // });

        };  

        function onError(evt) {
            document.getElementById('status').textContent = '连接失败:' + evt.data;
        };

        document.getElementById('messageInput').addEventListener('keyup', function(e) {
            
            var messageInput = document.getElementById('messageInput'),
                msg = messageInput.value,
                color = '#000000';
            if (e.keyCode == 13 && msg.trim().length != 0) {
                messageInput.value = '';
                var msgBin = "{\"sid\":1,\"rid\":2,\"type\":1,\"msg\":" + msg + "}";
                that.socket.send(msgBin);
                that._displayNewMsg('me', msg, color);
            };
        }, false);

        document.getElementById('sendBtn').addEventListener('click', function() {
            var messageInput = document.getElementById('messageInput'),
                msg = messageInput.value,
                color = '#000000';
            messageInput.value = '';
            messageInput.focus();
            if (msg.trim().length != 0) {
                var msgBin = "{\"sid\":1,\"rid\":2,\"type\":1,\"msg\":" + msg + "}";
                that.socket.send('postMsg', msg, color);
                that._displayNewMsg('me', msg, color);
                return;
            };
        }, false);

        document.getElementById('sendImage').addEventListener('change', function() {
            if (this.files.length != 0) {
                var file = this.files[0],
                    reader = new FileReader(),
                    color = '#000000';
                if (!reader) {
                    that._displayNewMsg('system', '!your browser doesn\'t support fileReader', 'red');
                    this.value = '';
                    return;
                };
                reader.onload = function(e) {
                    this.value = '';
                    that.socket.emit('img', e.target.result, color);
                    that._displayImage('me', e.target.result, color);
                };
                reader.readAsDataURL(file);
            };
        }, false);
        this._initialEmoji();
        document.getElementById('emoji').addEventListener('click', function(e) {
            var emojiwrapper = document.getElementById('emojiWrapper');
            emojiwrapper.style.display = 'block';
            e.stopPropagation();
        }, false);
        document.body.addEventListener('click', function(e) {
            var emojiwrapper = document.getElementById('emojiWrapper');
            if (e.target != emojiwrapper) {
                emojiwrapper.style.display = 'none';
            };
        });
        document.getElementById('emojiWrapper').addEventListener('click', function(e) {
            var target = e.target;
            if (target.nodeName.toLowerCase() == 'img') {
                var messageInput = document.getElementById('messageInput');
                messageInput.focus();
                messageInput.value = messageInput.value + '[emoji:' + target.title + ']';
            };
        }, false);

        document.getElementById('clearBtn').addEventListener('click', function() {
            document.getElementById('historyMsg').innerHTML = '';
        }, false);
    },        
        
        
    _initialEmoji: function() {
        var emojiContainer = document.getElementById('emojiWrapper'),
            docFragment = document.createDocumentFragment();
        for (var i = 69; i > 0; i--) {
            var emojiItem = document.createElement('img');
            emojiItem.src = '../static/content/emoji/' + i + '.gif';
            emojiItem.title = i;
            docFragment.appendChild(emojiItem);
        };
        emojiContainer.appendChild(docFragment);
    },
    _displayNewMsg: function(user, msg, color) {
        var container = document.getElementById('historyMsg'),
            msgToDisplay = document.createElement('p'),
            date = new Date().toTimeString().substr(0, 8),
            //determine whether the msg contains emoji
            msg = this._showEmoji(msg);
        msgToDisplay.style.color = color || '#000';
        msgToDisplay.innerHTML = user + '<span class="timespan">(' + date + '): </span>' + msg;
        container.appendChild(msgToDisplay);
        container.scrollTop = container.scrollHeight;
    },
    _displayImage: function(user, imgData, color) {
        var container = document.getElementById('historyMsg'),
            msgToDisplay = document.createElement('p'),
            date = new Date().toTimeString().substr(0, 8);
        msgToDisplay.style.color = color || '#000';
        msgToDisplay.innerHTML = user + '<span class="timespan">(' + date + '): </span> <br/>' + '<a href="' + imgData + '" target="_blank"><img src="' + imgData + '"/></a>';
        container.appendChild(msgToDisplay);
        container.scrollTop = container.scrollHeight;
    },
    _showEmoji: function(msg) {
        var match, result = msg,
            reg = /\[emoji:\d+\]/g,
            emojiIndex,
            totalEmojiNum = document.getElementById('emojiWrapper').children.length;
        while (match = reg.exec(msg)) {
            emojiIndex = match[0].slice(7, -1);
            if (emojiIndex > totalEmojiNum) {
                result = result.replace(match[0], '[X]');
            } else {
                result = result.replace(match[0], '<img class="emoji" src="../static/content/emoji/' + emojiIndex + '.gif" />');
            };
        };
        return result;
    }
};
