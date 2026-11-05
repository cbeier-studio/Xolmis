import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const out = path.join(root, 'architecture-map.html');
const layers = {
  data: ['Dados e serviços', 'Tipos, schema, filtros, busca e contratos compartilhados.'],
  providers: ['Providers SQL', 'Implementações de acesso ao banco e consultas por domínio.'],
  models: ['Models e repositórios', 'Entidades e operações de persistência dos registros.'],
  modules: ['Módulos', 'Controladores que conectam os dados às grades e às ações da aplicação.'],
  data_modules: ['Data modules', 'Conexões, consultas e datasets usados pelas telas.'],
  screens: ['Telas', 'Janelas e ferramentas principais usadas pelas pessoas.'],
  dialogs: ['Diálogos', 'Diálogos de configuração, seleção, importação e operações em lote.'],
  edit_dialogs: ['Edição de registros', 'Formulários para criar e editar registros do domínio.'],
  utils: ['Utilitários', 'Serviços compartilhados para permissões, GIS, mídia, tema, validação e outras tarefas.'],
  io: ['Importação e exportação', 'Leitura e gravação de dados em formatos de arquivo e integrações.'],
  reports: ['Relatórios (XRS)', 'Geração de relatórios HTML para visualização, impressão e exportação.'],
  tests: ['Testes', 'Testes automatizados das regras, dados, providers e importação/exportação.'],
  metadarkstyle: ['Tema escuro', 'Integração visual do tema escuro no Windows.'],
  root: ['Inicialização', 'Ponto de entrada e unidades globais da aplicação.'],
};
const excluded = new Set(['backup', 'lib', 'win64', 'output', 'bin']);
function walk(dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap(e => {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) return excluded.has(e.name.toLowerCase()) ? [] : walk(p);
    return /\.(pp|pas|lpr)$/i.test(e.name) ? [p] : [];
  });
}
function stripComments(s) {
  return s.replace(/\{\$[\s\S]*?\}|\{[\s\S]*?\}|\(\*[\s\S]*?\*\)|\/\/[^\n]*/g, ' ');
}
function getUses(s) {
  const found = new Set();
  for (const m of stripComments(s).matchAll(/\buses\b([\s\S]*?);/gi)) {
    const clause = m[1].replace(/\$[A-Za-z]+[^\n]*/g, ' ');
    for (const x of clause.matchAll(/(?:^|,)\s*([A-Za-z][\w.]*)\s*(?:in\s+['"][^'"]+['"])?\s*(?=,|$)/gim)) found.add(x[1].toLowerCase());
  }
  return [...found];
}
const files = walk(path.join(root, 'source'));
const units = new Map();
for (const file of files) {
  const src = fs.readFileSync(file, 'utf8');
  const m = stripComments(src).match(/\b(?:unit|program|library)\s+([A-Za-z][\w.]*)\s*;/i);
  if (!m) continue;
  const rel = path.relative(root, file).replaceAll('\\', '/');
  const parts = rel.split('/');
  const layer = parts[1] in layers ? parts[1] : 'root';
  units.set(m[1].toLowerCase(), { unit: m[1], path: rel, layer, uses: getUses(src) });
}
for (const u of units.values()) {
  u.dependencies = u.uses.filter(n => units.has(n) && n !== u.unit.toLowerCase());
  u.external = u.uses.filter(n => !units.has(n));
}
const counts = new Map();
for (const u of units.values()) for (const dep of u.dependencies) {
  const target = units.get(dep).layer;
  if (target !== u.layer) { const k = `${u.layer}\0${target}`; counts.set(k, (counts.get(k) || 0) + 1); }
}
const data = {
  units: [...units.values()].sort((a, b) => a.layer.localeCompare(b.layer) || a.unit.localeCompare(b.unit)),
  layers: Object.entries(layers).map(([id, [name, summary]]) => ({ id, name, summary })),
  edges: [...counts].map(([k, count]) => { const [from, to] = k.split('\0'); return { from, to, count }; }),
  stats: { units: units.size, dependencies: [...units.values()].reduce((n, u) => n + u.dependencies.length, 0), external: new Set([...units.values()].flatMap(u => u.external)).size },
};
const embedded = JSON.stringify(data).replaceAll('</', '<\\/');
const html = `<!doctype html>
<html lang="pt-BR"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Arquitetura do Xolmis</title>
<style>
:root{color-scheme:dark;--bg:#10151b;--panel:#18212a;--panel2:#202c37;--line:#334452;--text:#edf3f6;--muted:#a8bac5;--accent:#82d5bd;--blue:#92bcf5;--gold:#f1cc79}*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--text);font:15px/1.5 system-ui,Segoe UI,sans-serif}header{padding:28px clamp(18px,4vw,54px) 20px;border-bottom:1px solid var(--line);background:linear-gradient(125deg,#1c2b34,#121920 68%)}h1{margin:0;font-size:clamp(25px,4vw,38px);letter-spacing:-.03em}header p{max-width:850px;color:var(--muted);margin:8px 0 16px}.stats{display:flex;gap:10px;flex-wrap:wrap}.stat{padding:6px 10px;border:1px solid var(--line);border-radius:20px;color:var(--accent);font-size:13px}.wrap{max-width:1500px;margin:auto;padding:24px clamp(16px,3vw,42px)}.controls{display:flex;gap:12px;flex-wrap:wrap;align-items:center;margin-bottom:20px}input,select{background:var(--panel);color:var(--text);border:1px solid var(--line);padding:11px 13px;border-radius:9px;font:inherit}input{min-width:min(340px,100%);flex:1}.layout{display:grid;grid-template-columns:minmax(0,1.3fr) minmax(310px,.7fr);gap:18px;align-items:start}.box{background:var(--panel);border:1px solid var(--line);border-radius:13px;padding:18px}.box h2{font-size:17px;margin:0 0 12px}.flow{display:flex;gap:9px;overflow:auto;padding:6px 2px 14px;align-items:stretch}.layer{min-width:150px;flex:1;border:1px solid var(--line);background:var(--panel2);border-radius:10px;padding:12px;cursor:pointer;color:var(--text);text-align:left}.layer:hover,.layer.active{border-color:var(--accent);transform:translateY(-2px)}.layer strong{display:block;font-size:14px}.layer small{color:var(--muted);display:block;margin-top:6px}.layer .n{color:var(--accent);font-size:12px;margin-top:10px}.arrow{display:none}.note{font-size:12px;color:var(--muted)}.list{margin-top:18px;max-height:65vh;overflow:auto;display:grid;gap:7px}.unit{display:flex;justify-content:space-between;gap:14px;text-align:left;background:var(--panel2);border:1px solid transparent;border-radius:8px;color:var(--text);padding:10px 12px;cursor:pointer}.unit:hover,.unit.active{border-color:var(--blue)}.unit small{color:var(--muted);display:block}.badge{color:var(--accent);white-space:nowrap;font-size:12px}.detail{position:sticky;top:16px;min-height:280px}.detail h2{font-size:21px}.path{font:12px ui-monospace,Consolas,monospace;color:var(--blue);overflow-wrap:anywhere}.detail a{color:var(--accent)}.deps{display:flex;gap:6px;flex-wrap:wrap;margin:6px 0 15px}.deps button{background:#24343e;border:1px solid #405663;border-radius:16px;color:var(--text);padding:5px 9px;cursor:pointer}.deps button:hover{border-color:var(--accent)}.empty{color:var(--muted);padding:20px 0}.legend{margin-top:13px;color:var(--muted);font-size:12px}@media(max-width:850px){.layout{grid-template-columns:1fr}.detail{position:static;grid-row:1}.flow{flex-wrap:wrap}.arrow{display:none}}
</style></head><body><header><h1>Como o Xolmis se organiza</h1><p>Mapa gerado a partir das declarações <code>uses</code> dos arquivos Free Pascal. Selecione uma área para filtrar as unidades; abra uma unidade para ver o que ela usa e quem depende dela.</p><div class="stats" id="stats"></div></header><main class="wrap"><div class="controls"><input id="search" placeholder="Buscar unidade ou caminho…" aria-label="Buscar unidade ou caminho"><select id="filter"><option value="">Todas as áreas</option></select><span class="note">As ligações entre áreas são calculadas a partir das referências encontradas no código.</span></div><div class="layout"><section><div class="box"><h2>Áreas e dependências entre elas</h2><div class="flow" id="flow"></div><div class="legend">A ordem das áreas agrupa as camadas; as ligações contam dependências encontradas entre elas.</div></div><div class="box" style="margin-top:16px"><h2 id="title">Unidades Pascal</h2><div class="list" id="list"></div></div></section><aside class="box detail" id="detail"><div class="empty">Escolha uma unidade para ver suas conexões.</div></aside></div></main>
<script>const D=${embedded},$=s=>document.querySelector(s),esc=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c])),byLayer=new Map(D.layers.map(x=>[x.id,x]));let layer='',selected='';const count=Object.fromEntries(D.layers.map(x=>[x.id,D.units.filter(u=>u.layer===x.id).length]));$('#stats').innerHTML='<span class="stat">'+D.stats.units+' units Pascal</span><span class="stat">'+D.stats.dependencies+' dependências internas</span><span class="stat">'+D.stats.external+' units externas referenciadas</span>';$('#filter').innerHTML+=[...D.layers].filter(x=>count[x.id]).map(x=>'<option value="'+x.id+'">'+esc(x.name)+' ('+count[x.id]+')</option>').join('');$('#flow').innerHTML=D.layers.filter(x=>count[x.id]).map((x,i)=>{const links=D.edges.filter(e=>e.from===x.id||e.to===x.id).length;return(i?'<span class="arrow">›</span>':'')+'<button class="layer" data-layer="'+x.id+'"><strong>'+esc(x.name)+'</strong><small>'+esc(x.summary)+'</small><div class="n">'+count[x.id]+' units'+(links?' · '+links+' ligações a outras áreas':'')+'</div></button>'}).join('');function list(){const q=$('#search').value.toLowerCase().trim(),items=D.units.filter(u=>(!layer||u.layer===layer)&&(!q||u.unit.toLowerCase().includes(q)||u.path.toLowerCase().includes(q)));$('#title').textContent='Unidades Pascal ('+items.length+')';$('#list').innerHTML=items.map(u=>'<button class="unit '+(u.unit.toLowerCase()===selected?'active':'')+'" data-unit="'+esc(u.unit.toLowerCase())+'"><span><strong>'+esc(u.unit)+'</strong><small>'+esc(u.path)+'</small></span><span class="badge">'+u.dependencies.length+' usa · '+u.external.length+' externa</span></button>').join('')||'<div class="empty">Nenhuma unidade encontrada.</div>';document.querySelectorAll('.unit').forEach(b=>b.onclick=()=>detail(b.dataset.unit));document.querySelectorAll('.layer').forEach(b=>b.onclick=()=>{layer=layer===b.dataset.layer?'':b.dataset.layer;$('#filter').value=layer;document.querySelectorAll('.layer').forEach(x=>x.classList.toggle('active',x.dataset.layer===layer));list()})}function detail(key){const u=D.units.find(x=>x.unit.toLowerCase()===key);if(!u)return;selected=key;const incoming=D.units.filter(x=>x.dependencies.includes(key));$('#detail').innerHTML='<h2>'+esc(u.unit)+'</h2><div class="path">'+esc(u.path)+'</div><p>'+esc(byLayer.get(u.layer)?.name||'Xolmis')+' · '+u.dependencies.length+' dependências internas · '+incoming.length+' dependentes</p><h3>Usa ('+u.dependencies.length+')</h3><div class="deps">'+(u.dependencies.map(n=>'<button data-jump="'+esc(n)+'">'+esc(D.units.find(x=>x.unit.toLowerCase()===n)?.unit||n)+'</button>').join('')||'<span class="note">Nenhuma unit interna identificada.</span>')+'</div><h3>Usada por ('+incoming.length+')</h3><div class="deps">'+(incoming.map(x=>'<button data-jump="'+esc(x.unit.toLowerCase())+'">'+esc(x.unit)+'</button>').join('')||'<span class="note">Nenhuma referência interna encontrada.</span>')+'</div><h3>Bibliotecas e units externas ('+u.external.length+')</h3><div class="deps">'+(u.external.map(n=>'<span class="badge">'+esc(n)+'</span>').join('')||'<span class="note">Nenhuma.</span>')+'</div><p><a href="'+encodeURI(u.path)+'">Abrir arquivo no repositório ↗</a></p><p class="note">As dependências são inferidas das cláusulas <code>uses</code>. Condicionais, aliases e units incluídas por pacotes Lazarus podem exigir conferência.</p>';$('#detail').querySelectorAll('[data-jump]').forEach(b=>b.onclick=()=>detail(b.dataset.jump));list()}$('#search').addEventListener('input',list);$('#filter').addEventListener('change',e=>{layer=e.target.value;document.querySelectorAll('.layer').forEach(x=>x.classList.toggle('active',x.dataset.layer===layer));list()});list();</script></body></html>`;
fs.writeFileSync(out, html, 'utf8');
console.log(`Gerado ${path.relative(root, out)}: ${data.stats.units} units, ${data.stats.dependencies} dependências internas e ${data.stats.external} nomes de units externas.`);
