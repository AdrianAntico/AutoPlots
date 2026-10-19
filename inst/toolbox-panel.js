function(el, x) {
  if (el._apControlsCleanup) el._apControlsCleanup();
  const chart = window.echarts && window.echarts.getInstanceByDom(el);
  if (!chart) return;
  const native = (Array.isArray(x.opts.toolbox) ? x.opts.toolbox[0] : x.opts.toolbox) || {};
  if (native.show === false) return;
  let expanded = false, zoom = false;
  const bar = document.createElement('div');
  bar.className = 'ap-chart-controls';
  bar.setAttribute('role', 'group');
  bar.setAttribute('aria-label', 'Chart interaction');
  bar.style.cssText = 'position:absolute;bottom:0;left:0;right:0;display:flex;flex-wrap:wrap;gap:4px;padding:4px;background:#172033;color:#f3f6fc;z-index:3;box-sizing:border-box';
  el.style.position = 'relative';
  function button(label, run) {
    const b = document.createElement('button');
    b.type = 'button'; b.textContent = label;
    b.style.cssText = 'min-height:44px;padding:8px 12px;border:1px solid #8495b0;border-radius:6px;background:#25334c;color:#fff;font:inherit;cursor:pointer';
    b.addEventListener('click', run); bar.appendChild(b); return b;
  }
  const features = native.feature || {};
  const groups = Object.keys(features).filter(key => features[key] && features[key].show !== false);
  const group = document.createElement('select');
  group.setAttribute('aria-label', 'Advanced chart tool group');
  group.style.cssText = 'min-height:44px;max-width:100%;background:#25334c;color:white;border:1px solid #8495b0;border-radius:6px;padding:8px';
  const labels = {dataZoom:'Zoom tools', restore:'Restore', saveAsImage:'Image export',
    magicType:'Series presentation', dataView:'Data view', brush:'Selection tools'};
  groups.forEach(key => {const o=document.createElement('option');o.value=key;o.textContent=labels[key] || key;group.appendChild(o);});
  function showGroup() {
    const shown = {};
    groups.forEach(key => {shown[key] = Object.assign({}, features[key], {show:key === group.value});});
    chart.setOption({toolbox:{show:expanded, feature:shown, orient:'vertical', right:8, left:null,
      top:48, bottom:null, itemSize:22, itemGap:18, backgroundColor:'#172033',
      padding:8, iconStyle:{borderColor:'#fff'}, emphasis:{iconStyle:{borderColor:'#7dd3fc'}}}});
  }
  let zoomButton;
  function disableZoom() {
    if (features.dataZoom && features.dataZoom.show !== false) {
      // ECharts processes cursor changes only while this feature is rendered.
      chart.setOption({toolbox:{show:true,feature:{dataZoom:{show:true}}}});
      chart.dispatchAction({type:'takeGlobalCursor', key:'dataZoomSelect', dataZoomSelectActive:false});
    }
    zoom = false;
    if (zoomButton) zoomButton.setAttribute('aria-pressed','false');
  }
  if (features.dataZoom && features.dataZoom.show !== false) {
    zoomButton = button('Select zoom', function() {
      zoom = !zoom;
      if (zoom) {
        expanded = true; group.hidden = false; group.value = 'dataZoom';
        more.setAttribute('aria-expanded', 'true'); showGroup(); fit();
      }
      chart.dispatchAction({type:'takeGlobalCursor', key:'dataZoomSelect', dataZoomSelectActive:zoom});
      zoomButton.setAttribute('aria-pressed', String(zoom));
    });
    zoomButton.setAttribute('aria-pressed', 'false');
  }
  if (features.restore && features.restore.show !== false) button('Reset view', function() {
    chart.dispatchAction({type:'restore'});
    expanded = false; zoom = false;
    if (zoomButton) zoomButton.setAttribute('aria-pressed', 'false');
    more.setAttribute('aria-expanded', 'false');
    group.hidden = true;
    chart.setOption({toolbox:{show:false}}); fit();
  });
  if (features.saveAsImage && features.saveAsImage.show !== false) button('Save image', function() {
    const a = document.createElement('a'); a.download = 'chart.png';
    a.href = chart.getDataURL({type:'png', pixelRatio:2, excludeComponents:['toolbox']});
    a.click();
  });
  const more = button('More tools', function() {
    if (expanded) disableZoom();
    expanded = !expanded;
    more.setAttribute('aria-expanded', String(expanded));
    group.hidden = !expanded; showGroup(); fit();
  });
  more.setAttribute('aria-expanded', 'false');
  more.title = 'Show the existing native chart toolbox. Escape closes it.';
  group.hidden = true; group.addEventListener('change', function() {disableZoom(); showGroup();}); bar.appendChild(group);
  function escape(event) {
    if (event.key === 'Escape' && expanded) {
      disableZoom();
      expanded = false; chart.setOption({toolbox:{show:false}});
      group.hidden = true; more.setAttribute('aria-expanded','false'); more.focus(); fit();
    }
  }
  el.addEventListener('keydown', escape);
  el.appendChild(bar);
  chart.setOption({toolbox:{show:false}});
  function fit() {
    if (!el.isConnected || chart.isDisposed()) return;
    const height = Math.max(120, el.clientHeight - bar.offsetHeight - 4);
    if (chart.getHeight() !== height || chart.getWidth() !== el.clientWidth)
      chart.resize({width:el.clientWidth, height:height});
  }
  const observer = new ResizeObserver(fit); observer.observe(el); observer.observe(bar);
  chart.on('finished', fit);
  requestAnimationFrame(fit);
  el._apControlsCleanup = function() {
    observer.disconnect(); chart.off('finished',fit); el.removeEventListener('keydown',escape); bar.remove();
  };
}
