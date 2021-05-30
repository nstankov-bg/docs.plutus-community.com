import { MoonIcon, SunIcon } from "@chakra-ui/icons";
import {
  Box,
  Button,
  ButtonGroup,
  useColorMode,
  useColorModeValue,
} from "@chakra-ui/react";
import Link from "next/link";

const TopNav: React.FC<{ className?: string }> = ({ className }) => {
  const { toggleColorMode, colorMode } = useColorMode();
  const isDark = colorMode === "dark";
  const backgroundColor = useColorModeValue("gray.100", "gray.900");
  const shadow = useColorModeValue(
    " 1px 0px 6px 1px rgba(0,0,0,0.2)",
    "-1px 0px 4px 1px rgba(255,255,255,0.2)"
  );

  return (
    <Box
      className={className}
      backgroundColor={backgroundColor}
      css={{
        width: "100%",
        height: 52,
        flexShrink: 0,
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        boxShadow: shadow,
        zIndex: 98,
      }}
    >
      <Link href="/" passHref>
        <Button
          as="a"
          title="home"
          css={{
            left: 16,
            zIndex: 99,
            padding: 2,
          }}
        >
          <img src="/assets/plutus-logo.svg" alt="plutus logo" />
        </Button>
      </Link>
      <Box
        css={{
          display: "flex",
          right: 16,
          alignItems: "center",
          justifyContent: "space-evenly",
          width: 160,
        }}
      >
        <ButtonGroup>
          <Button
            as="a"
            href="https://github.com/nstankov-bg/docs.plutus-community.com"
            title="contribute"
            css={{
              padding: 8,
              borderRadius: "50%",
              width: 40,
              boxShadow: "1px 0px 6px 1px rgba(0,0,0,0.2)",
            }}
          >
            <img
              src={
                isDark
                  ? "/assets/github-logo-white.svg"
                  : "/assets/github-logo.svg"
              }
              alt="github logo"
            />
          </Button>
          <Button
            as="a"
            href="https://discord.com/invite/xzPWJC4D9B" // to be verified that it's ok to publish this invite link
            title="discord"
            css={{
              padding: 6,
              borderRadius: "50%",
              width: 40,
              boxShadow: "1px 0px 6px 1px rgba(0,0,0,0.2)",
            }}
          >
            <img
              src={
                isDark
                  ? "/assets/discord-logo.svg"
                  : "/assets/discord-logo-color.svg"
              }
              alt="discord logo"
            />
          </Button>
        </ButtonGroup>
        <Button
          title="toggle-color-mode"
          css={{
            padding: 2,
            borderRadius: "50%",
            boxShadow: "1px 0px 6px 1px rgba(0,0,0,0.2)",
          }}
          onClick={toggleColorMode}
        >
          {isDark ? <MoonIcon /> : <SunIcon color="yellow.500" />}
        </Button>
      </Box>
    </Box>
  );
};

export default TopNav;
