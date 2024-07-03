function is_empty(s)
  return s == nil or s == ''
end

function translation(lang)
  local result
  if (lang == "nl-BE") then
    result = {
      author = "Geschreven door",
      address = "Vlaamse overheid, Herman Teirlinckgebouw, Havenlaan 88, 1000 Brussel",
      citation = "Wijze van citeren",
      city = "Brussel",
      country = "België",
      depotnr = "Depotnummer",
      export = "Exporteer referentie als",
      location = "Vestiging",
      mission = "Hier komt de missie",
      name = "Vlaamse overheid",
      ordernr = "Opdrachtnummer",
      reviewer = "Nagelezen door",
      series = "Rapporten van het",
      url = "https://www.vlaanderen.be",
      url_text = "vlaanderen.be",
      vu = "Verantwoordelijke uitgever",
      year = "Gepubliceerd in"
    }
  elseif (lang == "fr-FR") then
    result = {
      author = "Écrit par",
      address = "Autorité flamande, Herman Teirlinckgebouw, Avenu du Port 88, 1000 Bruxelles",
      citation = "Citation recommandée",
      city = "Bruxelles",
      country = "Belgique",
      depotnr = "Numéro de dépôt",
      export = "Exporter la référence à",
      location = "Adresse",
      mission = "Mission statement",
      name = "l'Autorité flamande ",
      ordernr = "Numéro de commande",
      reviewer = "Examiné par",
      series = "Rapports de",
      url = "https://www.vlaanderen.be/fr",
      url_text = "vlaanderen.be",
      vu = "Éditeur responsable",
      year = "Publié en"
    }
  else
    result = {
      author = "Written by",
      address = "Government of Flanders, Herman Teirlinck, Havenlaan 88, 1000 Brussel",
      citation = "Way of quoting",
      city = "Brussels",
      country = "Belgium",
      depotnr = "Deposit number",
      export = "Export reference to",
      location = "Location:",
      mission = "Misson statement",
      name = "Government of Flanders",
      ordernr = "Order number",
      reviewer = "Reviewed by",
      series = "Reports of the",
      url = "https://www.vlaanderen.be/en",
      url_text = "vlaanderen.be",
      vu = "Responsible publisher",
      year = "Published during"
    }
  end
  return result
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
    res = res .. '<h1 class = "missing">!!! flandersqmd.' .. type ..' element ' .. i .. ' has no name element!!!</h1>'
  else
    if is_empty(person.name.family) then
      res = res .. '<h1 class = "missing">!!! flandersqmd.' .. type .. ' element ' .. i .. ' name element has no family element!!!</h1>'
    else
      res = res .. ' ' .. pandoc.utils.stringify(person.name.family)
    end
    if is_empty(person.name.given) then
      res = res .. '<h1 class = "missing">!!! flandersqmd.' .. type .. ' element ' .. i .. ' name element has no given element!!!</h1>'
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

return {
  {
    Meta = function(meta)
      meta.translation = translation(pandoc.utils.stringify(meta.lang))
      meta.issn = "ISSN nummer"
      meta.vu = "Naam verantwoordelijke uitgever"
      if is_empty(meta.flandersqmd.author) then
        meta.shortauthor = pandoc.RawInline(
          "html",
          '<h1 class = "missing">!!! Missing flandersqmd.author !!!</h1>'
        )
      else
        meta.shortauthor = shortauthor(meta.flandersqmd.author)
      end
      return meta
    end
  }
}
