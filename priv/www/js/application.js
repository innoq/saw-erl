(function($){
	$(function(){
		
		$("#text").keyup(function(){
			var canvas = document.getElementById("canvas");
			var context = canvas.getContext('2d');
			canvas.width = context.measureText($(this).val()).width;
			canvas.height = 8;
			context.fillStyle = '#f00';
			context.font = '8px monospace';
			context.textBaseline = 'top';
			context.fillText($(this).val(), 0, 0);
		})
		
		
	})
}(jQuery))