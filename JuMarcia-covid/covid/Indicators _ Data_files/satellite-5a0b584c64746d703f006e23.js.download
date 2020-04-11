_satellite.pushAsyncScript(function(event, target, $variables){
  /*YouTube Player updates required by Adobe Video Tracking module */
var n = 0;

if(typeof JQuery != 'undefined'){
jQuery('iframe').each(function () {

	var src = jQuery(this).attr('src');
	if (src) {
		if (src.indexOf('youtube.com') > -1) {

			if (src.indexOf('?') > -1) {
				if (src.indexOf('enablejsapi') == -1) {
					src = src + '&enablejsapi=1';
				}
			} else {
				src = src + '?enablejsapi=1';
			}

			jQuery(this).attr('src', src);
			jQuery(this).attr('id', 'ytplayer' + n);
			n++
		}
	}

});
}else{
  var iFrames = document.getElementsByTagName('iframe');
	for(i=0;i<iFrames.length;i++){
		var src = iFrames[i].getAttribute('src');
		if(src){
			if (src.indexOf('youtube.com') > -1){
				if (src.indexOf('?') > -1) {
					if (src.indexOf('enablejsapi') == -1) {
						src = src + '&enablejsapi=1';
					}
				}else{
					src = src + '?enablejsapi=1';
				}
				
				iFrames[i].setAttribute('src', src);
				iFrames[i].setAttribute('id', 'ytplayer' + n);
				n++
			}
		}
	}
}

if (n > 0) {
	/* Loads IFrame Player API Code asynchronously */
	var tag = document.createElement('script');
	tag.src = "https://www.youtube.com/iframe_api";
	var firstScriptTag = document.getElementsByTagName('script')[0];
	firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
	/* End YoutTube Player Mapping */

	/* YouTube Player Mapping (https://developers.google.com/youtube/iframe_api_reference) */
	var playerInfoList = new Array();

	if (document.getElementsByTagName('iframe')) {
		for (i = 0; i < document.getElementsByTagName('iframe').length; i++) {
			if (document.getElementsByTagName('iframe')[i].id.indexOf('ytplayer') > -1) {
				playerInfoList.push(document.getElementsByTagName('iframe')[i]);
			}
		}
	}

	var players = new Array();
	window.playersaem = players;
	window.onYouTubeIframeAPIReady = function () {
		for (x = 0; x < playerInfoList.length; x++) {
			players[x] = new YT.Player(playerInfoList[x], {
					events: {
						onStateChange: onPlayerStateChange
					}
				});
		}
	}
	
  //Player state change handler and video analytics tracker
	function onPlayerStateChange(event) {

    var videoPlayer = "YouTube";
    var mediaOffset = Math.floor(event.target.getCurrentTime());
    var mediaLength = Math.floor(event.target.getDuration());
    console.log("event: "+event.data);
    console.log("YT.PlayerState.PLAYING: "+YT.PlayerState.PLAYING);
    console.log("YT.PlayerState.PAUSED: "+YT.PlayerState.PAUSED);
    console.log("mediaOffset: "+mediaOffset);
    console.log("media length: "+mediaLength)
    var videoTitle = event.target.getVideoData().title;
    
    //console.log("media length: "+mediaLength);
    //console.log("mediaOffset: "+mediaOffset);
    //console.log("event.data: "+event.data);
    //console.log("YT.PlayerState.PLAYING: "+YT.PlayerState.PLAYING);
    //console.log("YT.PlayerState.PAUSED: "+YT.PlayerState.PAUSED);
    //console.log("YT.PlayerState.BUFFERING: "+YT.PlayerState.BUFFERING);
    //console.log("YT.PlayerState.ENDED: "+YT.PlayerState.ENDED);
    
    //wb.Media.playerName = videoPlayer;
    /*
    if (event.data == YT.PlayerState.PLAYING && mediaLength == 0) {
      videoContentType = "live:youtube video";  
      wb.media.trackSeconds = 60;
      wb.Media.open(videoTitle, mediaLength, videoPlayer);
      	console.log("60 seconds tracking set");
    }*/
    if (event.data == 3 && mediaOffset == 0 && mediaLength == 0) {
      videoContentType = "live:youtube video";  
      wb.Media.trackSeconds = 60;
      wb.Media.open(videoTitle, mediaLength, videoPlayer);
      wb.Media.play(videoTitle, mediaOffset);
      _satellite.getVar('FBPixelVideoLive');
      //console.log("60 seconds tracking set");
    }
    
    if (event.data == YT.PlayerState.PLAYING) {
			if (mediaOffset == 0 && mediaLength > 0) {
				videoContentType = "replay:youtube video";
        wb.Media.segmentByMilestones = true;
    		wb.Media.trackMilestones = '25,50,75';
      	//console.log("milestone tracking set");
        wb.Media.open(videoTitle, mediaLength, videoPlayer);
				wb.Media.play(videoTitle, mediaOffset);
        _satellite.getVar('FBPixelVideoReplay');
        _satellite.getVar('TwitterPixelVideoReplay');
        _satellite.getVar('GAdwordsTagVideoReplay');
			} else {
				wb.Media.play(videoTitle, mediaOffset);
			};
		}else if (event.data == YT.PlayerState.PAUSED) {
			wb.Media.stop(videoTitle, mediaOffset);
		}else if (event.data == YT.PlayerState.BUFFERING) {
			wb.Media.stop(videoTitle, mediaOffset);
		}else if (event.data == YT.PlayerState.ENDED) {
			wb.Media.stop(videoTitle, mediaOffset);
			wb.Media.close(videoTitle);
			mediaOffset = 0;
		}
	}
}
});
