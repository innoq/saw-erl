(function($){
	"use strict";

	$(function(){

		$("#text").keyup(function(){
			var canvas = document.getElementById("canvas");
			var context = canvas.getContext('2d');
			canvas.width = context.measureText($(this).val()).width;
			canvas.height = 8;
			context.fillStyle = '#f00';
			context.font = 'bold 14px monospace';
			context.textBaseline = 'alphabetic';
			context.fillText($(this).val(), 0, 8);
		})

		$("form").submit(function(e){
			e.preventDefault();
			var canvas = document.getElementById("canvas");
			var context = canvas.getContext('2d');
			var imageData = context.getImageData(0, 0, canvas.width, canvas.height);
			var res = [];
			for (var col = 0; col < imageData.width; col++) {
				var colByte = 0;
				for(var row = 0; row < imageData.height; row++) {
					var index = (row * imageData.width + col) * 4 + 0;
					if (imageData.data[index] > 200) {
						colByte = colByte | (1 << row);
					}
				}
				res.push(colByte);
			}
			$.post("http://localhost:8080/content", res.join(","))
			return false;
		});

	});

}(jQuery))