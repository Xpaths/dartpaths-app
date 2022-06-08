// transition for conditionalPanel

$(document).ready(function(){

	// appearance of loading panel for substances

	$('#loadingContainer').on('show', function(){
		$(this).css('opacity', '0').animate({opacity:1}, {duration: 100}
		);


	});
	
	// slow down appearance of substance search results for substances

	$('#substanceContainer').on('show', function(){
		$(this).css('opacity', '0').delay(600).animate({opacity:1}, {duration: 100}
		);
		
	
	});	
	
	// appearance of loading panel for categories
	
	$('#loadingContainerCat').on('show', function(){
		$(this).css('opacity', '0').animate({opacity:1}, {duration: 100}
		);


	});
	
	// slow down appearance of substance search results for categories

	$('#categoryContainer').on('show', function(){
		$(this).css('opacity', '0').delay(600).animate({opacity:1}, {duration: 100}
		);
		
	
	});	
	
});	