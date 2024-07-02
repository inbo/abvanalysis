function is_empty(s)
  return s == nil or s == ''
end

function translation_entity(result, lang, entity)
  if (entity == "INBO") then
    if (lang == "nl-BE") then
      result.address = "INBO Brussel, Herman Teirlinckgebouw, Havenlaan 88 bus 73, 1000 Brussel"
      result.city = "Brussel"
      result.mission = "Het INBO is het onafhankelijk onderzoeksinstituut van de Vlaamse overheid dat via toegepast wetenschappelijk onderzoek, data- en kennisontsluiting het biodiversiteitsbeleid en -beheer onderbouwt en evalueert."
      result.name = "Instituut voor Natuur- en Bosonderzoek"
      result.series = "Rapporten van het"
      result.tagline = "vlaanderen-wetenschap.pdf"
      result.url = "https://www.vlaanderen.be/inbo"
      result.url_text = "vlaanderen.be/inbo"
    elseif (lang == "fr-FR") then
      result.address = "INBO Bruxelles, Herman Teirlinckgebouw, Avenu du Port 88 boîte 73, 1000 Bruxelles"
      result.city = "Bruxelles"
      result.mission = "l'Institut de Recherche sur la Nature et les Forêts ('Instituut voor Natuur- en Bosonderzoek', INBO) est un institut de recherche indépendant du gouvernement flamand, qui étaye et évalue la politique et la gestion en matière de biodiversité par la recherche scientifique appliquée, l'intégration et la dissémination publique de données et de connaissances."
      result.name = "l'Institut de Recherche sur la Nature et les Forêts"
      result.series = "Rapports de"
      result.tagline = "flanders-state-art.pdf"
      result.url = "https://www.vlaanderen.be/inbo/en-gb/homepage/"
      result.url_text = "vlaanderen.be/inbo"
    else
      result.address = "INBO Brussels, Herman Teirlinckgebouw, Havenlaan 88 bus 73, 1000 Brussels"
      result.city = "Brussels"
      result.mission = "The Research Institute for Nature and Forest (INBO) is an independent research institute of the Flemish government. Through applied scientific research, open data and knowledge, integration and disclosure, it underpins and evaluates biodiversity policy and management."
      result.name = "Research Instute for Nature and Forest"
      result.tagline = "flanders-state-art.pdf"
      result.url = "https://www.vlaanderen.be/inbo/en-gb/homepage/"
      result.url_text = "vlaanderen.be/inbo"
    end
    result.issn_nr = "1782-9054"
    result.vu_name = "Hilde Eggermont"
  else
    result.address = metadata_warning('flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity')
    result.city = metadata_warning('flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity')
    result.mission = metadata_warning('flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity')
    result.name = metadata_warning('flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity')
    result.tagline = "flanders-state-art.pdf"
    result.url = ''
    result.url_text = metadata_warning('flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity')
    result.issn_nr = metadata_warning("flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity")
    result.vu_name = metadata_warning("flandersqmd.entity must be equal to INBO. Please contact the maintainer if you need a different entity")
  end
  return result
end

function translation(lang, entity)
  local result
  if (lang == "nl-BE") then
    result = {
      author = "Geschreven door",
      author_pdf = "Auteurs",
      ccby = "Dit werk valt onder een \\href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Naamsvermelding 4.0 Internationaal-licentie}.",
      citation = "Wijze van citeren",
      city = "Brussel",
      client = "Dit onderzoek werd uitgevoerd in opdracht van",
      cooperation = "Dit onderzoek werd uitgevoerd in samenwerking met",
      country = "België",
      coverdescription = "Foto cover",
      depotnr = "Depotnummer",
      export = "Exporteer referentie als",
      location = "Vestiging",
      mission = "Hier komt de missie",
      ordernr = "Opdrachtnummer",
      reviewer = "Nagelezen door",
      reviewer_pdf = "Reviewers",
      series = "Rapporten van het",
      vu = "Verantwoordelijke uitgever",
      year = "Gepubliceerd in"
    }
  elseif (lang == "fr-FR") then
    result = {
      author = "Écrit par",
      author_pdf = "Auteurs",
      ccby = "Ce rapport est sous licence \\href{https://creativecommons.org/licenses/by/4.0/deed.fr}{Creative Commons Attribution 4.0 International}.",
      citation = "Citation recommandée",
      city = "Bruxelles",
      client = "Cette étude a été commandée par",
      cooperation = "Cette étude a été menée en collaboration avec",
      country = "Belgique",
      coverdescription = "Photo de couverture",
      depotnr = "Numéro de dépôt",
      export = "Exporter la référence à",
      location = "Adresse",
      mission = "Mission statement",
      ordernr = "Numéro de commande",
      reviewer = "Examiné par",
      reviewer_pdf = "Reviewers",
      series = "Rapports de",
      vu = "Éditeur responsable",
      year = "Publié en"
    }
  else
    result = {
      author = "Written by",
      author_pdf = "Authors",
      ccby = "This work is licensed under a \\href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 Generic License}.",
      citation = "Way of quoting",
      city = "Brussels",
      client = "This study was commissioned by",
      cooperation = "This study was conducted in collaboration with",
      country = "Belgium",
      coverdescription = "Cover photo",
      depotnr = "Deposit number",
      export = "Export reference to",
      location = "Location",
      mission = "Misson statement",
      ordernr = "Order number",
      reviewer = "Reviewed by",
      reviewer_pdf = "Reviewers",
      series = "Reports of the",
      vu = "Responsible publisher",
      year = "Published during"
    }
  end
  return translation_entity(result, lang, entity)
end

function abbreviate_person(person, i, type, n)
  if i > 2 then
    return ''
  end
  if i == 2 and n > 2 then
    return ' et al.'
  end
  if (i > 1) then
    res = ' & '
  else
    res = ''
  end
  if is_empty(person.name) then
    res = res .. metadata_warning('flandersqmd.' .. type ..' element ' .. i .. ' has no name element')
  else
    if is_empty(person.name.family) then
      res = res .. metadata_warning('flandersqmd.' .. type .. ' element ' .. i .. ' name element has no family element')
    else
      res = res .. ' ' .. pandoc.utils.stringify(person.name.family)
    end
    if is_empty(person.name.given) then
      res = res .. metadata_warning('flandersqmd.' .. type .. ' element ' .. i .. ' name element has no given element')
    else
      local x = pandoc.utils.stringify(person.name.given)
      res = res .. ', ' .. string.gsub(x, '([A-Z])[a-zàçéèïîôö]*', '%1.')
    end
  end
  return res
end

function shortauthor(author)
  n = 0
  for i, person in pairs(author) do
    n = i
  end
  z = ''
  for i, person in pairs(author) do
    z = z .. abbreviate_person(person, i, 'author', n)
  end
  return z
end

function metadata_warning(warningmessage)
  if FORMAT:match 'latex' then
    return '{\\color{red}!!! ' .. warningmessage .. ' !!!}'
  else
    return '<h1 class = "missing">!!! ' .. warningmessage .. ' !!!</h1>'
  end
end

function conditional_format(txt)
  if FORMAT:match 'latex' then
    return pandoc.RawInline('latex', txt)
  else
    return pandoc.RawInline('html', txt)
  end
end

function publicationyear(pubdate)
  local pattern = "^(%d%d%d%d)-(%d%d)-(%d%d+)$"
  local runyear, runmonth, runday = pubdate:match(pattern)
  if runyear == nil then
    z = metadata_warning("flandersqmd.publicationdate not in YYYY-MM-DD format")
  else
    local ts = os.time({year = runyear, month = runmonth, day = runday})
    z = os.date("%Y", ts)
  end
  return z
end

return {
  {
    Meta = function(meta)
      if is_empty(meta.flandersqmd.entity) then
        meta.translation = translation(
          pandoc.utils.stringify(meta.lang), "INBO"
        )
      else
        meta.translation = translation(
          pandoc.utils.stringify(meta.lang),
          pandoc.utils.stringify(meta.flandersqmd.entity)
        )
      end
      if is_empty(meta.flandersqmd.author) then
        meta.shortauthor = conditional_format(
          metadata_warning('Missing flandersqmd.author')
        )
      else
        meta.shortauthor = shortauthor(meta.flandersqmd.author)
        meta.ccby = pandoc.RawInline("latex", meta.translation.ccby)
      end
      if not is_empty(meta.flandersqmd.publicationdate) then
        meta.publicationyear = publicationyear(
          pandoc.utils.stringify(meta.flandersqmd.publicationdate)
        )
        meta.fpublicationyear = conditional_format(meta.publicationyear)
      end
      return meta
    end
  }
}
