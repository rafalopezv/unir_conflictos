/*
 Highcharts JS v8.0.0 (2019-12-10)
 Organization chart series type

 (c) 2019-2019 Torstein Honsi

 License: www.highcharts.com/license
*/
(function(b){"object"===typeof module&&module.exports?(b["default"]=b,module.exports=b):"function"===typeof define&&define.amd?define("highcharts/modules/organization",["highcharts","highcharts/modules/sankey"],function(g){b(g);b.Highcharts=g;return b}):b("undefined"!==typeof Highcharts?Highcharts:void 0)})(function(b){function g(b,g,r,t){b.hasOwnProperty(g)||(b[g]=t.apply(null,r))}b=b?b._modules:{};g(b,"modules/organization.src.js",[b["parts/Globals.js"],b["parts/Utilities.js"]],function(b,g){var r=
g.pick,t=g.wrap,q=b.seriesTypes.sankey.prototype;b.seriesType("organization","sankey",{borderColor:"#666666",borderRadius:3,linkRadius:10,borderWidth:1,dataLabels:{nodeFormatter:function(){function a(a){return Object.keys(a).reduce(function(c,d){return c+d+":"+a[d]+";"},'style="')+'"'}var c={width:"100%",height:"100%",display:"flex","flex-direction":"row","align-items":"center","justify-content":"center"},e={"max-height":"100%","border-radius":"50%"},d={width:"100%",padding:0,"text-align":"center",
"white-space":"normal"},b={margin:0},f={margin:0},h={opacity:.75,margin:"5px"};this.point.image&&(e["max-width"]="30%",d.width="70%");this.series.chart.renderer.forExport&&(c.display="block",d.position="absolute",d.left=this.point.image?"30%":0,d.top=0);c="<div "+a(c)+">";this.point.image&&(c+='<img src="'+this.point.image+'" '+a(e)+">");c+="<div "+a(d)+">";this.point.name&&(c+="<h4 "+a(b)+">"+this.point.name+"</h4>");this.point.title&&(c+="<p "+a(f)+">"+(this.point.title||"")+"</p>");this.point.description&&
(c+="<p "+a(h)+">"+this.point.description+"</p>");return c+"</div></div>"},style:{fontWeight:"normal",fontSize:"13px"},useHTML:!0},hangingIndent:20,linkColor:"#666666",linkLineWidth:1,nodeWidth:50,tooltip:{nodeFormat:"{point.name}<br>{point.title}<br>{point.description}"}},{pointAttribs:function(a,c){var e=this,d=q.pointAttribs.call(e,a,c),b=e.mapOptionsToLevel[(a.isNode?a.level:a.fromNode.level)||0]||{},f=a.options,h=b.states&&b.states[c]||{};c=["borderRadius","linkColor","linkLineWidth"].reduce(function(a,
c){a[c]=r(h[c],f[c],b[c],e.options[c]);return a},{});a.isNode?c.borderRadius&&(d.r=c.borderRadius):(d.stroke=c.linkColor,d["stroke-width"]=c.linkLineWidth,delete d.fill);return d},createNode:function(a){a=q.createNode.call(this,a);a.getSum=function(){return 1};return a},createNodeColumn:function(){var a=q.createNodeColumn.call(this);t(a,"offset",function(a,e,d){a=a.call(this,e,d);return e.hangsFrom?{absoluteTop:e.hangsFrom.nodeY}:a});return a},translateNode:function(a,c){q.translateNode.call(this,
a,c);a.hangsFrom&&(a.shapeArgs.height-=this.options.hangingIndent,this.chart.inverted||(a.shapeArgs.y+=this.options.hangingIndent));a.nodeHeight=this.chart.inverted?a.shapeArgs.width:a.shapeArgs.height},curvedPath:function(a,c){var e=[],d;for(d=0;d<a.length;d++){var b=a[d][0];var f=a[d][1];if(0===d)e.push("M",b,f);else if(d===a.length-1)e.push("L",b,f);else if(c){var h=a[d-1][0];var g=a[d-1][1];var m=a[d+1][0];var k=a[d+1][1];if(h!==m&&g!==k){var l=h<m?1:-1;var n=g<k?1:-1;e.push("L",b-l*Math.min(Math.abs(b-
h),c),f-n*Math.min(Math.abs(f-g),c),"C",b,f,b,f,b+l*Math.min(Math.abs(b-m),c),f+n*Math.min(Math.abs(f-k),c))}}else e.push("L",b,f)}return e},translateLink:function(a){var c=a.fromNode,b=a.toNode,d=Math.round(this.options.linkLineWidth)%2/2,g=Math.floor(c.shapeArgs.x+c.shapeArgs.width)+d,f=Math.floor(c.shapeArgs.y+c.shapeArgs.height/2)+d,h=Math.floor(b.shapeArgs.x)+d,p=Math.floor(b.shapeArgs.y+b.shapeArgs.height/2)+d,m=this.options.hangingIndent;var k=b.options.offset;var l=/%$/.test(k)&&parseInt(k,
10),n=this.chart.inverted;n&&(g-=c.shapeArgs.width,h+=b.shapeArgs.width);k=Math.floor(h+(n?1:-1)*(this.colDistance-this.nodeWidth)/2)+d;l&&(50<=l||-50>=l)&&(k=h=Math.floor(h+(n?-.5:.5)*b.shapeArgs.width)+d,p=b.shapeArgs.y,0<l&&(p+=b.shapeArgs.height));b.hangsFrom===c&&(this.chart.inverted?(f=Math.floor(c.shapeArgs.y+c.shapeArgs.height-m/2)+d,p=b.shapeArgs.y+b.shapeArgs.height):f=Math.floor(c.shapeArgs.y+m/2)+d,k=h=Math.floor(b.shapeArgs.x+b.shapeArgs.width/2)+d);a.plotY=1;a.shapeType="path";a.shapeArgs=
{d:this.curvedPath([[g,f],[k,f],[k,p],[h,p]],this.options.linkRadius)}},alignDataLabel:function(a,c,g){if(g.useHTML){var d=a.shapeArgs.width,e=a.shapeArgs.height,f=this.options.borderWidth+2*this.options.dataLabels.padding;this.chart.inverted&&(d=e,e=a.shapeArgs.width);e-=f;d-=f;b.css(c.text.element.parentNode,{width:d+"px",height:e+"px"});b.css(c.text.element,{left:0,top:0,width:"100%",height:"100%",overflow:"hidden"});c.getBBox=function(){return{width:d,height:e}}}b.seriesTypes.column.prototype.alignDataLabel.apply(this,
arguments)}});""});g(b,"masters/modules/organization.src.js",[],function(){})});
//# sourceMappingURL=organization.js.map