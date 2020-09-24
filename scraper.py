def covid_scraper():
	'''
	Cette fonction scrape le site covid.info et retourne les données jourlières 
	relatives au covid-19 en cote d'Ivoire
	'''
	# modules
	from requests import get
	from bs4 import BeautifulSoup
	import pandas as pd
	url = "https://covid19-ci.info/"
	response = get(url)
	soup = BeautifulSoup(response.content, "html.parser")
		# cas confirmés
	cas_confirme_1 = soup.find("div", class_ = "ul-widget__content")
	confirmes = cas_confirme_1.h4.text.replace('\n','').replace('  ','')
	cas = soup.find_all("div", class_ = "ul-widget__content")

	# Ensemble des cas
	cas = soup.find_all("div", class_ = "ul-widget__content")
	cas_titre = [cas[i].p.text for i in range(len(cas))]
	cas_chiffre = [cas[i].h4.text.replace('\n','').replace('  ','') for i in range(len(cas))]
	cas_df = pd.DataFrame({cas_titre[i] : cas_chiffre[i] for i in range(len(cas)) },index = range(1))

	# Regions touchees
	region_touchee = soup.find_all("ul", class_ = "list-group")[0]
	region_touchee_chiffres = region_touchee.find_all("span", class_ = "text-success")
	region_touchee_chiffre = [region_touchee.find_all("span", class_ = "text-success")[i].text 
	                          for i in range(len(region_touchee_chiffres))]

	x = region_touchee.find_all("li", \
	                            class_ = "list-group-item d-flex justify-content-between align-items-center")
	y = region_touchee.find_all("li", \
	                class_ = "list-group-item d-flex justify-content-between align-items-center rmsk")

	region_touchee_1 = [x[i].text for i in range(len(x))]
	region_touchee_1 = [region_touchee_1[i].split(' ')[-2] for i in range(len(x))]

	region_touchee_chiffre_1 = [x[i].span.text for i in range(len(x))]

	region_touchee_2 = [y[i].text for i in range(len(y))] 
	region_touchee_2 = [region_touchee_2[i].split(' ')[-2] for i in range(len(y))]

	region_touchee_chiffre_2 = [y[i].span.text for i in range(len(y))]
	region_touchee_chiffre = region_touchee_chiffre_1 +  region_touchee_chiffre_2
	region_touchee = region_touchee_1 +  region_touchee_2
	region_touchee_df = pd.DataFrame({"Regions":region_touchee,"Cas_confirmes":region_touchee_chiffre})
	region_touchee_df.to_csv("data//region_touchees.csv",index=False)

	# Villes touchees
	zone_ville = soup.find_all("ul")[2]
	ville_1 = zone_ville.find_all("li",
	                                class_ = "list-group-item d-flex justify-content-between align-items-center")
	ville_2 = zone_ville.find_all("li",
	                                class_ = "list-group-item d-flex justify-content-between align-items-center vmsk")
	ville_touchee  = ville_1 + ville_2
	ville_nom = [ville_touchee[i].text.split(" ")[-2] for i in range(len(ville_touchee))]
	ville_chiffre = [ville_touchee[i].text.split(" ")[-1] for i in range(len(ville_touchee))]
	ville_touchee_df = pd.DataFrame({"Villes":ville_nom,"Cas_confirmes":ville_chiffre})
	ville_touchee_df.to_csv("data//ville_touchees.csv",index=False)

	# Donnes sur les communes
	# les identifiants des communes d'abidjan
	commune_id = ['ABJ01','ABJ02','ABJ03','ABJ04','ABJ05','ABJ06','ABJ07','ABJ08','ABJ09','ABJ10']
	commune = ['Abobo','Attécoubé','Adjamé','Cocody','Yopougon','Plateau','Marcory','Koumassi','Treichville',
	           'Port-Bouët']
	
	vil= str(soup.find_all('script')[-4]).split('var resvil =')
	ab = vil[1].split()[4].split('null')[:-1]
	v = vil[1].split("}}")[0].split('null')[:-1]
	def zz(x):
	    k = ''.join([i for i in list(x) if i.isdigit() ])
	    return k

	def zzz(x):
	    k = [zz(i) for i in x]
	    return [i.replace(' ',' ') for i in k if i not in ['',',',';',']','[',"{" "}" ] ]

	def zdate(x):
	    return [i[:4]+'-'+i[4:6]+ '-' + i[6:8] for i in zzz(x)]

	def ccc(ab):
	        ab = ab.replace('{','').replace('}','').replace('"','').replace(',','').replace(' ','').split(':')
	        ab_k = ab[0]
	        ab_v = zzz(ab[1:])
	        return {ab_k : ab_v}
	vil_chiffre = [ccc(v[i]) for i in range(len(v))] + [ccc(ab[i]) for i in range(len(ab))]
	def dic(d):
	    dc = {}
	    for i in range(len(d)):
	        for u,v in d[i].items() :
	            dc[u]=v
	    return dc

	vil_chiffres = dic(vil_chiffre)
	commune_df = pd.DataFrame(columns = ["Commune","Cas_confirmes","Gueris","Deces"])
	tmp_df = pd.DataFrame(columns = ["Commune","Cas_confirmes","Gueris","Deces"])
	for code in commune_id:
	    
	    tmp_df["Commune"] = commune[commune_id.index(code)]
	    tmp_df["Cas_confirmes"] = [int(vil_chiffres[code][0])]
	    tmp_df["Gueris"] = int(vil_chiffres[code][1])
	    tmp_df["Deces"] = int(vil_chiffres[code][2])
	    commune_df = pd.concat([commune_df,tmp_df],axis=0) 
	commune_df.iloc[0,0] = "Abobo"
	commune_df.to_csv("data//cas_commune.csv",index=False)

	# Rapport jourliers des cas
	data = str(soup.find_all('script')[-2]).split('var datas = ')[1].split(";")[0].split()
	### Recupérztion des dates
	# 1ere recuperation
	data1 = data[0]
	data1 = zdate(data1.split(','))
	# 2ème recuperation
	data2 = data[1]
	data2_1 = data2.split('[')[0].split(',')
	data2_1 = zdate(data2_1)
	# cas confirmes
	data2_2 = data2.split('[')[1].split(',')
	data2_2 = zzz(data2_2)
	cas_confirmes = [int(i) for i in data2_2]
	# cas gueris
	data2_3  = data2.split('[')[2].split(',')
	data2_3 = zzz(data2_3)
	cas_gueris = [int(i) for i in data2_3]
	# Echantillons journaliers
	data2_4  = data2.split('[')[3].split(',')
	data2_4 = zzz(data2_4)
	echant = [int(i) for i in data2_4]
	# Deces
	data2_5  = data2.split('[')[4].split(',')
	data2_5 = zzz(data2_5)
	data2_5[-5:]
	deces  = [int(i) for i in data2_5]
	# Cas actifs journalier
	data2_6  = data2.split('[')[5].split(',')
	data2_6 = zzz(data2_6)
	cas_actif = [int(i) for i in data2_6]
	# Cas sains dans les echantillons journaliers
	data2_7  = data2.split('[')[6].split(',')
	data2_7 = zzz(data2_7)
	cas_sains = [int(i) for i in data2_7]
	# ensemble des dates
	dates = data1 + data2_1
	# df journalier total
	df_journaliers = pd.DataFrame({"date":dates,"confirmes":cas_confirmes,"cas_actifs":cas_actif,"gueris":cas_gueris,
	                            "echantillons":echant,"cas_sain_echan":cas_sains})
	df_journaliers.to_csv("data//rapport_journalier.csv",index=False)

	print("################# Terminé avec succès")


if __name__ == "__main__":
    covid_scraper()