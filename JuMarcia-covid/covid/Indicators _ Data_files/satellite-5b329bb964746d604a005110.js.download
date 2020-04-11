_satellite.pushAsyncScript(function(event, target, $variables){
  if (document.getElementsByTagName('video').length > 0){console.log("html5 video found");
	var playerInfoList = new Array();

	if (document.getElementsByTagName('video')) {
		for (i = 0; i < document.getElementsByTagName('video').length; i++) {
			//if (document.getElementsByTagName('video')[i].id.indexOf('html5player') > -1) {
				playerInfoList.push(document.getElementsByTagName('video')[i]);
			//}
		}
	}

	for (x = 0; x < playerInfoList.length; x++) {
		playerInfoList[x].addEventListener("play", AnalyticsHandler);
		playerInfoList[x].addEventListener("seeked", AnalyticsHandler);
		playerInfoList[x].addEventListener("seeking", AnalyticsHandler);
		playerInfoList[x].addEventListener("pause", AnalyticsHandler);
		playerInfoList[x].addEventListener("ended", AnalyticsHandler);
	}

	//HTML5 Video Event Handler and Tracker
	function AnalyticsHandler(e){
		var video = e.target;
		var vSrc = video.currentSrc;
    //console.log("vSrc: "+vSrc);
		var mediaName = vSrc.split("/")[vSrc.split("/").length-1].split(".")[0];
		var mediaLength = Math.floor(video.duration);
		var mediaOffset = video.currentTime>0?Math.floor(video.currentTime):0;
		var mediaPlayername = "html5 video player";
		
		if(e.type === "play"){
		  if(mediaOffset == 0){
			videoContentType = "replay:dynamic media video";
			wb.Media.segmentByMilestones = true;
			wb.Media.trackMilestones = '25,50,75';
			wb.Media.open(mediaName,mediaLength,mediaPlayername);
			wb.Media.play(mediaName,mediaOffset);
			_satellite.getVar('FBPixelVideoReplay');
			_satellite.getVar('TwitterPixelVideoReplay');
			_satellite.getVar('GAdwordsTagVideoReplay');
		  }else{
			wb.Media.play(mediaName,mediaOffset);
		  }
		}else if(e.type === "seeking" || e.type === "pause"){
		  wb.Media.stop(mediaName,mediaOffset);
		}else if(e.type === "seeked"){
		  wb.Media.play(mediaName,mediaOffset);
		}else if(e.type === "ended"){
		  wb.Media.stop(mediaName,mediaOffset);
		  wb.Media.close(mediaName);
		  mediaOffset = 0;
		}
	}
}
});
