_satellite.pushAsyncScript(function(event, target, $variables){
  var n = 0;

if(typeof JQuery != 'undefined'){
jQuery('iframe').each(function () {

	var src = jQuery(this).attr('src');
	if (src) {
		if (src.indexOf('ustream.tv') > -1) {
			jQuery(this).attr('src', src);
			jQuery(this).attr('id', 'UstreamIframe' + n);
			n++
		}
	}

});
}else{
  var iFrames = document.getElementsByTagName('iframe');
	for(i=0;i<iFrames.length;i++){
		var src = iFrames[i].getAttribute('src');
		if(src){
			if (src.indexOf('ustream.tv') > -1){
				iFrames[i].setAttribute('src', src);
				iFrames[i].setAttribute('id', 'UstreamIframe' + n);
				n++
			}
		}
	}
}

if(n>0){
	/** include the ustream player API **/

	var UstreamEmbed=function(){function a(a){return b(a)}function b(a){var b=c(a),m=function(a){function b(b){if("socialstream"!==b){if(!v)return x||(x=[]),x.push(arguments),void 0;var d=i(arguments).slice(1);d[0]&&"function"==typeof d[0]&&(y[b]||(y[b]=[]),y[b].push(d[0])),g(a,s,{cmd:b,args:d})}else if(j(window,"message",n),u=c(arguments[1]),t=h(u.getAttribute("src")),w=!0,A.length)for(var e=0,k=A.length;k>e;e++)f(A[e])}function m(){if(x){for(;x.length;)b.apply(this,x.shift());x=null}}function n(a){var b=u;b&&b.contentWindow&&b.contentWindow===a.source?r.onmessage(a):a.source===u.id&&r.onmessage(a)}function o(a){var c,d=JSON.parse(a.data);return d.cmd&&"ready"==d.cmd?(g(u,t,{cmd:"ready"}),void 0):(c=[d.cmd],c=c.concat(d.args),b.apply(this,c),void 0)}function p(){v=!0,m()}function q(){b.apply(this,arguments)}var r,s,t,u,v=!1,w=!1,x=[],y={},z={},A=[];return s=h(a.getAttribute("src")),r={host:s,callMethod:q,getProperty:function(){q.apply(this,arguments)},addListener:function(a,b){z[a]||(z[a]=[]),z[a].push(b)},removeListener:function(a,b){if(b)for(var c=0,d=z[a].length;d>c;c++)z[a][c]===b&&z[a].splice(c,1);else z[a]=null},onmessage:function(a){var b;if(s||t||A.push({origin:a.origin,data:a.data}),a.origin==s){try{b=JSON.parse(a.data)}catch(c){return}if(b.sstream)return o(a),void 0;if(b.event&&b.event.ready&&(p(),d(z,"ready")),b.event&&b.event.live===!0)return d(z,"live"),void 0;if(b.event&&b.event.live===!1)return d(z,"offline"),void 0;if(b.event&&!b.event.ready)if(k)Object.keys(b.event).forEach(function(a){d(z,a,b.event[a])});else for(var f in b.event)b.event.hasOwnProperty(f)&&d(z,f,b.event[f]);if(b.property)if(k)Object.keys(b.property).forEach(function(a){e(y,a,b.property[a])});else for(var f in b.property)b.property.hasOwnProperty(f)&&e(y,f,b.property[f])}else if(w&&a.origin==t)return o(a),void 0},destroy:function(){v=!1,s="",w=!1,t="",u=null,x=[],y={},z={},A=[],l[a.id]&&(l[a.id]=null),a=null}}}(b);return b.id||(b.id="UstreamEmbed"+Math.ceil(1e5*Math.random())),m.id=b.id,l[b.id]=m,m}function c(a){return"string"==typeof a&&(a=document.getElementById(a)),a}function d(a,b,c){for(var d in a[b])a[b].hasOwnProperty(d)&&a[b][d].call(window,b,c)}function e(a,b,c){if(a[b]){for(var d in a[b])a[b].hasOwnProperty(d)&&a[b][d].call(window,c);a[b]=null,delete a[b]}}function f(a){var b,c;for(b in l)l.hasOwnProperty(b)&&l[b]&&(c=document.getElementById(b),c&&c.contentWindow?c.contentWindow===a.source&&l[b].onmessage(a):"string"==typeof a.source&&a.source==b&&l[b].onmessage(a))}function g(a,b,c){a.contentWindow.postMessage(JSON.stringify(c),b)}function h(a){return a.indexOf("http")<0&&(a=location.protocol+a),a.match(m)[1].toString()}function i(a){return Array.prototype.slice.call(a,0)}function j(a,b,c){a.addEventListener?a.addEventListener(b,c,!1):a.attachEvent("on"+b,c)}var k="undefined"!=typeof Object.keys,l={},m=new RegExp("^(http(?:s)?://[^/]+)","im");return j(window,"message",f),window.UstreamEmbed=a}();
	
	/** check for the multiple players and assign the event listners to each **/
	if (document.getElementsByTagName('iframe')) {
		for (i = 0; i < document.getElementsByTagName('iframe').length; i++) {
			if (document.getElementsByTagName('iframe')[i].id.indexOf('UstreamIframe') > -1) {
				var iFrame = document.getElementsByTagName('iframe')[i].id;
				
				var viewer = UstreamEmbed(document.getElementsByTagName('iframe')[i].id);
				var videoType = "channels";
				var videoTitle;
				var mediaLength;
				var mediaOffset = 0;
				viewer.getProperty('content', function (content) {
					if(content[0] === "recorded"){videoType = "videos";}
					else if(content[0] === "channels"){videoType = "channels";}
					var videoId = content[1];
					videoTitle = getVideoTitle(videoId,videoType)[0];
					mediaLength = getVideoTitle(videoId,videoType)[1];
				});
								
				
				/** set event listeners for the ustream player **/
				viewer.addListener('playing', onEmbedEvent);
				viewer.addListener('finished', onEmbedEvent);
				viewer.addListener('offline', onEmbedEvent);
			}
		}
	}
	
	var prevEvent = "";
	var startTime=0;

	/** Ustream Media Tracking handler **/
	function onEmbedEvent(event, data){
	  var videoPlayer = "ustream tv";
	  
	  if(data && event === 'playing' && prevEvent != event){
		  if(prevEvent !== "paused"){
			
			if(videoType === 'channels'){
				videoContentType = "live:ustream video";
				wb.Media.trackSeconds = 60;
        _satellite.getVar('FBPixelVideoLive');
			}
			else if(videoType === 'videos'){
				videoContentType = "replay:ustream video";
				wb.Media.segmentByMilestones = true;
				wb.Media.trackMilestones = '25,50,75';
				_satellite.getVar('FBPixelVideoReplay');
				_satellite.getVar('TwitterPixelVideoReplay');
				_satellite.getVar('GAdwordsTagVideoReplay');
			}
			wb.Media.open(videoTitle, mediaLength, videoPlayer);
			wb.Media.play(videoTitle, mediaOffset);
			startTime = new Date();
		  }else if(prevEvent === "paused" ){
			wb.Media.play(videoTitle, mediaOffset);
			startTime = new Date();
		  }
		  prevEvent = event;
	  }else if(!data && event === 'playing' && prevEvent != "paused"){
		  mediaOffset += Math.round((new Date() - startTime)/1000);
		  wb.Media.stop(videoTitle, mediaOffset);
		  prevEvent = "paused";		  
	  }else if(data && (event === 'finished' || event === 'offline')){
		  mediaOffset += Math.round((new Date() - startTime)/1000);
		  wb.Media.stop(videoTitle, mediaOffset);
		  wb.Media.close(videoTitle);
		  mediaOffset = 0;
		  prevEvent = event;
	  }
	}
	/** Get the video title by calling ustream RESTful service **/
	function getVideoTitle(videoId,videoType){
		var videoTitle = videoId;
		var videoLength = 0;
		var xhr = new XMLHttpRequest();
		
		xhr.onreadystatechange = function(){
			if(this.readyState == 4 && this.status == 200){
				var response = JSON.parse(xhr.responseText);
				if(videoType === 'videos'){
					videoTitle = response.video.title;
					videoLength = Math.round(response.video.length);
				}
				else if(videoType === 'channels'){videoTitle = response.channel.title;}
			}
		}
		xhr.open("GET", "https://api.ustream.tv/"+videoType+"/"+videoId+".json", false);
		xhr.send();
		
		return [videoTitle, videoLength];
	}
}
});
