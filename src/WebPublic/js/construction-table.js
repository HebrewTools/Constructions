"use strict";

function setClickHandler (header,child_headers,rows)
{
	header.dataset.expanded='true';
	header.onclick=() => {
		if (header.dataset.expanded=='true'){
			header.dataset.expanded='false';
			for (var r of rows)
				r.style.display='none';
		} else {
			header.dataset.expanded='true';
			for (var r of rows)
				r.style.display='table-row';
		}

		for (var child_header of child_headers)
			child_header.dataset.expanded=header.dataset.expanded;
	};
}

function initConstructionTable (domEl)
{
	var ngroups=0;
	for (var th of domEl.querySelectorAll ('thead th')){
		if (th.innerText == 'Reference')
			break;
		ngroups++;
	}

	const rows=domEl.querySelector ('tbody').childNodes;

	const groups=[];

	for (var row of rows){
		if (row.classList.contains ('header')){
			row.title='Click to collapse/expand these results';

			var hn=null;
			for (var j=0; j<ngroups; j++){
				if (row.childNodes[j].childNodes.length>0){
					hn=j;
					break;
				}
			}

			for (var j=0; j<hn; j++)
				groups[j].child_headers.push (row);

			if (hn in groups)
				setClickHandler (groups[hn].header,groups[hn].child_headers,groups[hn].rows);

			groups[hn]={
				header: row,
				child_headers: [],
				rows: []
			};
		} else {
			for (var j=0; j<ngroups; j++){
				groups[j].rows.push (row);
			}
		}
	}

	for (var g of groups)
		setClickHandler (g.header,g.child_headers,g.rows);
}
