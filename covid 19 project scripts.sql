/*
Covid 19 Data Exploration 
Skills used: Joins, CTE's, Temp Tables, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types
*/




SELECT *
FROM portfolioproject..coviddeaths
WHERE continent is not null
ORDER BY 3, 4;


-- Select Data that we are going to be starting with

SELECT location,date, total_cases, new_cases, total_deaths, population
FROM portfolioproject..coviddeaths
WHERE continent is not null
ORDER BY 1, 2;

-- looking at Total Cases vs Total Deaths
-- Shows likelihood of dying if you contract covid in your country

SELECT location,date, total_cases,total_deaths, (total_deaths/total_cases) * 100 as DeathPercentage
FROM portfolioproject..coviddeaths
WHERE location like 'italy%'
AND continent is not null
ORDER BY 1, 2;

-- looking at Total Cases vs Population
-- Shows what percentage of population infected with Covid

SELECT location,date,population, total_cases, ( total_cases/population) * 100 as PercentPopulationInfected
FROM portfolioproject..coviddeaths
WHERE location like 'italy%'
ORDER BY 1, 2;

--Looking at country with the highest infection Rate compared to Population

SELECT location,population, MAX(total_cases) AS HighestinfectionCount, MAX((total_cases/population)) * 100 as PercentPopulationInfected
FROM portfolioproject..coviddeaths
--where location like 'italy%'
GROUP BY location,population
ORDER BY PercentPopulationInfected DESC;

--Showing Countries with Highest Death Count Per Population

SELECT location, MAX(cast(total_deaths as bigint)) as TotalDeathCount
FROM portfolioproject..coviddeaths
--where location like 'italy%'
WHERE continent is not null
AND location not in ('World', 'European Union', 'International')
GROUP BY location
ORDER BY TotalDeathCount DESC;


-- LET'S BREAK THINGS DOWN BY CONTINENT


-- Showing contintents with the highest death count per population

SELECT continent, MAX(cast(total_deaths as bigint)) as TotalDeathCount
FROM portfolioproject..coviddeaths
--where location like 'italy%'
WHERE continent is not null
GROUP BY continent
ORDER BY TotalDeathCount DESC;


-- Global numbers

SELECT date, SUM(new_cases) as total_cases, SUM(cast(total_deaths as int)) as total_death, SUM(cast(new_deaths as int)) / SUM(New_Cases) * 100 as DeathPercentage
FROM portfolioproject..coviddeaths
-- location like 'italy%'
WHERE continent is not null
GROUP BY date
ORDER BY 1, 2;


--CHECKING COVIDVACCINATIONS AND COVIDDEATHS TABLES BY JOIN
Select *
From portfolioproject..coviddeaths dea
join portfolioproject..covidvaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date;



-- Looking at Total Population vs Vaccinations
-- Shows Percentage of Population that has recieved at least one Covid Vaccine

SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
dea.date) as RollingPeopleVaccinated
FROM portfolioproject..coviddeaths dea
JOIN portfolioproject..covidvaccinations vac
ON dea.location = vac.location
AND dea.date = vac.date
WHERE dea.continent is not null
ORDER BY 2,3;


-- Using CTE to perform Calculation on Partition By in previous query

With PopvsVac  (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as
(
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
dea.date) as RollingPeopleVaccinated
FROM portfolioproject..coviddeaths dea
JOIN portfolioproject..covidvaccinations vac
ON dea.location = vac.location
AND dea.date = vac.date
WHERE dea.continent is not null
--order by 2,3
)
SELECT *, (RollingPeopleVaccinated/population)*100
FROM PopvsVac;


-- Using Temp Table to perform Calculation on Partition By in previous query

DROP Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
New_vaccinations numeric,
RollingPeopleVaccinated numeric
)

INSERT INTO #PercentPopulationVaccinated
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(CONVERT(bigint,vac.new_vaccinations)) OVER (Partition by dea.location Order by dea.location,
dea.date) as RollingPeopleVaccinated
FROM portfolioproject..coviddeaths dea
JOIN portfolioproject..covidvaccinations vac
ON dea.location = vac.location
AND dea.date = vac.date
WHERE dea.continent is not null
--order by 2,3

SELECT *, (RollingPeopleVaccinated/population)*100
FROM #PercentPopulationVaccinated;



--LOOKING AT POPULATION vs TEST

--New tests vs Population

SELECT dea.location, dea.date, dea.population, vac.new_tests, vac.positive_rate, (vac.new_tests/dea.population) *100 AS New_Test_population_percent
FROM portfolioproject..coviddeaths dea
JOIN portfolioproject..covidvaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent is not null
ORDER BY 1,2;

--Total Test vs Population

SELECT dea.location, dea.date, dea.population, vac.total_tests, vac.positive_rate, (vac.total_tests/dea.population) *100 AS Total_Test_population_percent
FROM portfolioproject..coviddeaths dea
JOIN portfolioproject..covidvaccinations vac
	ON dea.location = vac.location
	AND dea.date = vac.date
WHERE dea.continent is not null
ORDER BY 1,2;



-- Creating View to store data for later visualizations 

CREATE VIEW Covid_cases AS
SELECT location,date, total_cases, new_cases, total_deaths, population
FROM portfolioproject..coviddeaths
WHERE continent is not null
--order by 1, 2;


CREATE VIEW DeathPercentage AS
SELECT location,date, total_cases,total_deaths, (total_deaths/total_cases) * 100 as DeathPercentage
FROM portfolioproject..coviddeaths
WHERE continent is not null
--order by 1, 2;

CREATE VIEW PercentPopulationInfected AS
SELECT location,date,population, total_cases, ( total_cases/population) * 100 as PercentPopulationInfected
FROM portfolioproject..coviddeaths
WHERE continent is not null
--order by 1, 2;

