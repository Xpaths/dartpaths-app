$(document).ready(function() {

	// trigger actionButton on Enter key (name search)

	var inputName = document.getElementById("substance_name");
	inputName.addEventListener('keyup', function(e) {
    	if (e.key == "Enter") {
		e.preventDefault();
    		$("#substance_search").click();
		}
	});
	
	// trigger actionButton on Enter key (cas search)
	
	var inputCas = document.getElementById("substance_cas");
	inputCas.addEventListener('keyup', function(e) {
    	if (e.key == "Enter") {
		e.preventDefault();
    		$("#substance_search").click();
		}
	});
	
	// trigger actionButton on Enter key (ec search)
	
	var inputEc = document.getElementById("substance_ec");
	inputEc.addEventListener('keyup', function(e) {
    	if (e.key == "Enter") {
		e.preventDefault();
    		$("#substance_search").click();
		}
	});
	
	// go to next form item on arrowRight key (CAS search)
	
	var inputCas1 = document.getElementById("substance_cas1");
	inputCas1.addEventListener('keyup', function(e) {
	if (e.key == "ArrowRight") {
		e.preventDefault();
		var nextInput = document.getElementById("substance_cas2");
		nextInput.focus();	
		}	
	});
	
	var inputCas2 = document.getElementById("substance_cas2");
	inputCas2.addEventListener('keyup', function(e) {
	if (e.key == "ArrowRight") {
		e.preventDefault();
		var nextInput = document.getElementById("substance_cas3");
		nextInput.focus();	
		}	
	});
	
	// go to previous form item on arrowLeft key (CAS search)
	
	var inputCas3 = document.getElementById("substance_cas3");
	inputCas3.addEventListener('keyup', function(e) {
	if (e.key == "ArrowLeft") {
		e.preventDefault();
		var previousInput = document.getElementById("substance_cas2");
		previousInput.focus();	
		}	
	});
	
	var inputCas2 = document.getElementById("substance_cas2");
	inputCas2.addEventListener('keyup', function(e) {
	if (e.key == "ArrowLeft") {
		e.preventDefault();
		var previousInput = document.getElementById("substance_cas1");
		previousInput.focus();	
		}	
	});
	
	// go to next form item on arrowRight key (EC search)
	
	var inputEc1 = document.getElementById("substance_ec1");
	inputEc1.addEventListener('keyup', function(e) {
	if (e.key == "ArrowRight") {
		e.preventDefault();
		var nextInput = document.getElementById("substance_ec2");
		nextInput.focus();	
		}	
	});
	
	var inputEc2 = document.getElementById("substance_ec2");
	inputEc2.addEventListener('keyup', function(e) {
	if (e.key == "ArrowRight") {
		e.preventDefault();
		var nextInput = document.getElementById("substance_ec3");
		nextInput.focus();	
		}	
	});
	
	// go to previous form item on arrowLeft key (EC search)
	
	var inputEc3 = document.getElementById("substance_ec3");
	inputEc3.addEventListener('keyup', function(e) {
	if (e.key == "ArrowLeft") {
		e.preventDefault();
		var previousInput = document.getElementById("substance_ec2");
		previousInput.focus();	
		}	
	});
	
	var inputEc2 = document.getElementById("substance_ec2");
	inputEc2.addEventListener('keyup', function(e) {
	if (e.key == "ArrowLeft") {
		e.preventDefault();
		var previousInput = document.getElementById("substance_ec1");
		previousInput.focus();	
		}	
	});		

});