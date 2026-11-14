-- Bibliography in the style of the Philebus project (philebus-bibliography.lua),
-- adapted for the textbook (a Quarto book).
--
-- Entries with BibLaTeX langid = russian get the Russian locale (В:, ред.,
-- «…», «и»), all others keep the English one. Unlike the Philebus filter,
-- in-text citations of Russian works are localized too («Мастицкий и Шитиков
-- 2015», not «Мастицкий and Шитиков 2015»).
--
-- A Quarto book normally rebuilds the references page after rendering with a
-- plain `pandoc --citeproc` call that ignores filters and lang, so Russian
-- entries would come out in English there. This filter therefore takes over
-- the book-level bibliography itself:
--   * in chapters it hides the chapter's own list (kept for citation pop-ups)
--     and points citation links to references.html, as Quarto does;
--   * on the page with `bibliography-page: true` in its front matter
--     (references.qmd) it lists every work cited in the book's .qmd files.
-- references.qmd must not contain a `::: {#refs}` div: that div is what makes
-- Quarto run its own pass.
--
-- In _quarto.yml: citeproc: false (this filter runs citeproc itself),
-- csl: chicago-author-date-ru.csl, filters: [bibliography-ru.lua].
local stringify = pandoc.utils.stringify
local references_page = 'references.html'
local quoted_title_types = {
  ['article-journal'] = true,
  ['article-magazine'] = true,
  ['article-newspaper'] = true,
  ['paper-conference'] = true,
  chapter = true
}

local function russian(reference)
  local lang = stringify(reference.language or ''):lower()
  return lang == 'ru' or lang == 'russian' or lang:match('^ru[-_]') ~= nil
end

local function inner_quotes(inlines)
  -- The outer title quotes are supplied by CSL. Quoted names inside a Russian
  -- title need the second level, e.g. «Ложные удовольствия в „Филебе“».
  return pandoc.Inlines(inlines):walk {
    Str = function(el)
      el.text = el.text:gsub('«', '„'):gsub('»', '“')
      return el
    end
  }
end

local function collect_cites(doc)
  local cites = {}
  doc:walk {
    Cite = function(el)
      cites[#cites + 1] = el
    end
  }
  return cites
end

local function all_russian(cite, russian_keys)
  for _, citation in ipairs(cite.citations) do
    if not russian_keys[citation.id] then return false end
  end
  return true
end

local function book_dir()
  local input = quarto and quarto.doc and quarto.doc.input_file
  return input and pandoc.path.directory(input) or '.'
end

-- Keys of all works cited anywhere in the book's .qmd files, in the order
-- they first occur. Only keys present in the bibliography count, so e-mail
-- addresses and R code with `@` are ignored.
local function cited_in_book(known)
  local dir = book_dir()
  local files = {}
  for _, name in ipairs(pandoc.system.list_directory(dir)) do
    if name:match('%.qmd$') then files[#files + 1] = name end
  end
  table.sort(files)
  local seen, keys = {}, {}
  for _, name in ipairs(files) do
    local handle = io.open(pandoc.path.join { dir, name }, 'r')
    if handle then
      local text = handle:read('a')
      handle:close()
      for key in text:gmatch('@([^%s%[%]{}();,]+)') do
        key = key:gsub('[%.:]+$', '')
        if known[key] and not seen[key] then
          seen[key] = true
          keys[#keys + 1] = key
        end
      end
    end
  end
  return keys
end

local function add_nocite(doc, keys)
  if #keys == 0 then return end
  local parts = {}
  for _, key in ipairs(keys) do parts[#parts + 1] = '@' .. key end
  local cites = pandoc.read('[' .. table.concat(parts, '; ') .. ']', 'markdown')
  doc.meta.nocite = pandoc.MetaInlines(pandoc.utils.blocks_to_inlines(cites.blocks))
end

local function citeproc_localized(doc)
  local saved_lang = doc.meta.lang
  local references = pandoc.utils.references(doc)
  local russian_keys, russian_ids = {}, {}
  for _, reference in ipairs(references) do
    if russian(reference) then
      local id = stringify(reference.id)
      russian_keys[id] = true
      russian_ids['ref-' .. id] = true
    end
  end

  local base = doc:clone()
  base.meta.lang = pandoc.MetaString('en-US')
  base = pandoc.utils.citeproc(base)
  base.meta.lang = saved_lang
  if next(russian_ids) == nil then return base end

  local localized = doc:clone()
  localized.meta.lang = pandoc.MetaString('ru-RU')
  for _, reference in ipairs(references) do
    if russian(reference) and reference.title
        and quoted_title_types[stringify(reference.type)] then
      reference.title = inner_quotes(reference.title)
    end
  end
  localized.meta.references = references
  localized.meta.bibliography = nil
  localized = pandoc.utils.citeproc(localized)

  -- Both passes see the same document, so their citations come in the same
  -- order; take the Russian rendering of every citation of Russian works only.
  local localized_cites = collect_cites(localized)
  local entries = {}
  localized:walk {
    Div = function(el)
      if russian_ids[el.identifier] then
        entries[el.identifier] = el
      end
    end
  }

  local n = 0
  return base:walk {
    Cite = function(el)
      n = n + 1
      local other = localized_cites[n]
      if other and all_russian(el, russian_keys) then
        return other
      end
    end,
    Div = function(el)
      return entries[el.identifier]
    end
  }
end

local function point_to_references_page(doc)
  return doc:walk {
    Cite = function(cite)
      return cite:walk {
        Link = function(link)
          if link.target:match('^#ref%-') then
            link.target = references_page .. link.target
            return link
          end
        end
      }
    end,
    Div = function(el)
      if el.identifier == 'refs' then
        el.attributes.style = 'display: none'
        return el
      end
    end
  }
end

function Pandoc(doc)
  local is_bibliography_page = doc.meta['bibliography-page'] == true
  if is_bibliography_page then
    local everything = doc:clone()
    add_nocite(everything, { '*' })
    local known = {}
    for _, reference in ipairs(pandoc.utils.references(everything)) do
      known[stringify(reference.id)] = true
    end
    add_nocite(doc, cited_in_book(known))
  end
  doc = citeproc_localized(doc)
  if not is_bibliography_page then
    doc = point_to_references_page(doc)
  end
  return doc
end
