_satellite.pushAsyncScript(function(event, target, $variables){
  if (typeof kWidget !== "undefined") {
	kWidget.addReadyCallback(function (playerId) {
	var kdp = document.getElementById(playerId);

	//console.log("player "+kdp);
	//console.log("Name "+kdp.evaluate('{mediaProxy.entry.name}'));
	//console.log("Duration "+kdp.evaluate('{duration}'));
	//console.log("Current Time "+kdp.evaluate('{video.player.currentTime}'));
	/*
	kdp.kBind('playerStateChange', function (event) {
		console.log("event "+event);
		kulturaPlayerHandler("playing", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});*/
	kdp.kBind("playerPlayed", function (MediaPlayerState) {
			kulturaPlayerHandler("playing", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});
		kdp.kBind("seek", function (MediaPlayerState) {
			kulturaPlayerHandler("seeking", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});
		kdp.kBind("seeked", function (MediaPlayerState) {
			kulturaPlayerHandler("seeked", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});
		kdp.kBind("playerPaused", function (MediaPlayerState) {
			kulturaPlayerHandler("paused", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});
		kdp.kBind("playerPlayEnd", function (MediaPlayerState) {
			kulturaPlayerHandler("ended", kdp.evaluate('{mediaProxy.entry.name}'), kdp.evaluate('{duration}'), kdp.evaluate('{video.player.currentTime}'));
		});
	});
}

function kulturaPlayerHandler(playerState,mediaName,videoDuration,videoCurrentTime){
	var videoPlayer = "Kaltura";
	var mediaOffset = videoCurrentTime>0?Math.floor(videoCurrentTime):0;
	var mediaLength = Math.floor(videoDuration);

	wb.Media.playerName = videoPlayer;

	//console.log("mediaOffset - "+mediaOffset);
	//console.log("mediaLength - "+mediaLength);
	//console.log("playerState - "+playerState);

	if(playerState == "playing" && mediaLength == 0){
	videoContentType = "live:kaltura video";
	wb.Media.trackSeconds = 60;
	wb.Media.open(mediaName, mediaLength, videoPlayer);
	//wb.Media.play(mediaName, mediaOffset);*/
	}

	if(playerState == "playing"){
		if (mediaOffset == 0 && mediaLength > 0){
			videoContentType = "replay:kaltura video";
			wb.Media.segmentByMilestones = true;
			wb.Media.trackMilestones = '25,50,75';
			//console.log("milestone tracking set");
			wb.Media.open(mediaName, mediaLength, videoPlayer);
			wb.Media.play(mediaName, mediaOffset);
			_satellite.getVar('FBPixelVideoReplay');
      _satellite.getVar('TwitterPixelVideoReplay');
      _satellite.getVar('GAdwordsTagVideoReplay');
		}else{
			wb.Media.play(mediaName, mediaOffset);
		}
	}else if(playerState == "seeking"){
		wb.Media.stop(mediaName, mediaOffset);
	}else if(playerState == "seeked"){
		wb.Media.play(mediaName, mediaOffset);
	}else if(playerState == "paused"){
		wb.Media.stop(mediaName, mediaOffset);
	}else if(playerState == "ended"){
		wb.Media.stop(mediaName, mediaOffset);
		wb.Media.close(mediaName);
		mediaOffset = 0;
	}
}
});
